defmodule Nova.Compiler.Types do
  @moduledoc """
  Core type representations used by the Hindley‑Milner type‑checker.
  """

  # A type variable is identified by an integer id (for fast comparisons)
  defmodule TVar do
    defstruct [:id, :name]
    def new(id, name), do: %__MODULE__{id: id, name: name}
  end

  # A concrete type constructor with zero or more parameters
  defmodule TCon do
    defstruct [:name, :args]
    def new(name, args \\ []), do: %__MODULE__{name: name, args: args}
  end

  @type t :: TVar.t() | TCon.t()

  # Convenience helpers for the common built‑ins
  def t_int, do: TCon.new(:Int)
  def t_string, do: TCon.new(:String)
  def t_char, do: TCon.new(:Char)
  def t_bool, do: TCon.new(:Bool)
  def t_list(el), do: TCon.new(:List, [el])
  def t_tuple(list), do: TCon.new({:Tuple, length(list)}, list)
  def t_arrow(a, b), do: TCon.new(:Fun, [a, b])

  # ---------------------------------------------------------------------------
  # Substitutions (Map from TVar.id => Types.t())
  # ---------------------------------------------------------------------------
  defmodule Subst do
    @type t :: %{optional(integer()) => Nova.Compiler.Types.t()}
    def new, do: %{}

    def lookup(sub, %TVar{id: id} = v), do: Map.get(sub, id, v)
    def lookup(_sub, t), do: t

    def single(%TVar{id: id}, t), do: %{id => t}

    def compose(s1, s2),
      do: Map.merge(Enum.into(s2, %{}, fn {k, v} -> {k, s_apply(s1, v)} end), s1)

    def s_apply(sub, %TVar{} = v), do: lookup(sub, v)

    def s_apply(sub, %TCon{args: args} = c) do
      %TCon{c | args: Enum.map(args, &s_apply(sub, &1))}
    end
  end

  # ---------------------------------------------------------------------------
  # Type Schemes (∀ quantified vars . type)
  # ---------------------------------------------------------------------------
  defmodule Scheme do
    defstruct [:vars, :type]
    def new(vars, type), do: %__MODULE__{vars: vars, type: type}
  end

  # ---------------------------------------------------------------------------
  # Environment mapping identifiers -> Scheme
  # ---------------------------------------------------------------------------
  defmodule Env do
    defstruct [:m, :counter]
    def empty, do: %__MODULE__{m: %{}, counter: 0}

    def extend(%__MODULE__{m: m} = e, name, scheme),
      do: %__MODULE__{e | m: Map.put(m, name, scheme)}

    def lookup(%__MODULE__{m: m}, name), do: Map.fetch(m, name)

    def fresh_var(%__MODULE__{counter: c} = e, hint \\ "t") do
      {TVar.new(c, String.to_atom(hint <> Integer.to_string(c))), %__MODULE__{e | counter: c + 1}}
    end
  end

  # ---------------------------------------------------------------------------
  # Utility functions
  # ---------------------------------------------------------------------------
  def free_type_vars(%TVar{} = v), do: MapSet.new([v.id])

  def free_type_vars(%TCon{args: args}),
    do: Enum.reduce(args, MapSet.new(), fn t, acc -> MapSet.union(acc, free_type_vars(t)) end)

  def free_type_vars_scheme(%Scheme{vars: q, type: t}) do
    MapSet.difference(free_type_vars(t), MapSet.new(Enum.map(q, & &1.id)))
  end

  def free_type_vars_env(%Env{m: m}) do
    m
    |> Map.values()
    |> Enum.reduce(MapSet.new(), fn s, acc -> MapSet.union(acc, free_type_vars_scheme(s)) end)
  end
end

# ============================================================================
# Unification
# ============================================================================

defmodule Nova.Compiler.Unify do
  alias Nova.Compiler.Types.{TVar, TCon, Subst}

  @spec unify(Types.t(), Types.t()) :: {:ok, Subst.t()} | {:error, String.t()}
  def unify(a, b), do: do_unify(a, b, Subst.new())

  defp do_unify(%TVar{id: i} = v, t, s) do
    case Subst.lookup(s, v) do
      ^v -> bind(i, t, s)
      t2 -> do_unify(t2, t, s)
    end
  end

  defp do_unify(t, %TVar{} = v, s), do: do_unify(v, t, s)

  defp do_unify(%TCon{name: n1, args: a1}, %TCon{name: n2, args: a2}, s)
       when n1 == n2 and length(a1) == length(a2) do
    Enum.zip(a1, a2)
    |> Enum.reduce_while({:ok, s}, fn {x, y}, {:ok, sub} ->
      case do_unify(Subst.s_apply(sub, x), Subst.s_apply(sub, y), sub) do
        {:ok, sub2} -> {:cont, {:ok, sub2}}
        err -> {:halt, err}
      end
    end)
  end

  defp do_unify(t1, t2, _s), do: {:error, "Cannot unify #{inspect(t1)} with #{inspect(t2)}"}

  defp occurs?(i, %TVar{id: j}), do: i == j
  defp occurs?(i, %TCon{args: args}), do: Enum.any?(args, &occurs?(i, &1))

  defp bind(i, t, _s) when is_struct(t, TVar) and t.id == i, do: {:ok, Subst.new()}

  defp bind(i, t, _s) do
    cond do
      occurs?(i, t) ->
        {:error, "Occurs check failed"}

      true ->
        {:ok, %{i => t}}
    end
  end
end

# ============================================================================
# Hindley‑Milner algorithm W
# ============================================================================

defmodule Nova.Compiler.TypeChecker do
  @moduledoc """
  A minimal Hindley‑Milner type‑checker (algorithm W) for the Nova AST.
  At this stage we support literals, identifiers, lambda, let‑binding,
  function application and a handful of built‑in operators.

  Each public entry point returns either `{:ok, type, env}` or
  `{:error, reason}`.
  """

  alias Nova.Compiler.Unify
  alias Nova.Compiler.Types
  alias Types.{Env, Scheme, Subst}
  alias Nova.Compiler.Ast

  # Top‑level API --------------------------------------------------------------
  def check_module(%Ast.Module{declarations: decls}, env \\ Env.empty()) do
    Enum.reduce_while(decls, {:ok, env}, fn decl, {:ok, e} ->
      case check_declaration(decl, e) do
        {:ok, e2} -> {:cont, {:ok, e2}}
        err -> {:halt, err}
      end
    end)
  end

  def check_declaration(%Ast.FunctionDeclaration{} = f, env) do
    with {:ok, t, s, e2} <- infer_function(f, env) do
      env2 = generalize_bind(f.name, t, s, e2)
      {:ok, env2}
    end
  end

  def check_declaration(%Ast.TypeSignature{} = sig, env) do
    {:ok, Env.extend(env, sig.name, Scheme.new([], convert_type(sig.type)))}
  end

  # TODO handle
  def check_declaration(%Ast.ImportDeclaration{}, env), do: {:ok, env}
  def check_declaration(_, _), do: {:error, "Declaration kind not yet supported"}

  # ---------------------------------------------------------------------------
  # Expression inference
  # ---------------------------------------------------------------------------
  def infer_expression(%Ast.Literal{type: :number}, env),
    do: {:ok, Types.t_int(), Subst.new(), env}

  def infer_expression(%Ast.Literal{type: :string}, env),
    do: {:ok, Types.t_string(), Subst.new(), env}

  def infer_expression(%Ast.Literal{type: :char}, env),
    do: {:ok, Types.t_char(), Subst.new(), env}

  def infer_expression(%Ast.Identifier{name: n}, env) do
    with {:ok, scheme} <- Env.lookup(env, n) do
      {:ok, instantiate(scheme, env), Subst.new(), env}
    else
      :error -> {:error, "Unbound identifier #{n}"}
    end
  end

  def infer_expression(%Ast.Lambda{parameters: params, body: body}, env) do
    {param_types, env1} =
      Enum.map_reduce(params, env, fn p, e ->
        {v, e2} = Env.fresh_var(e, to_string(p.name || "p"))
        {v, Env.extend(e2, p.name, Scheme.new([], v))}
      end)

    with {:ok, body_type, s1, env2} <- infer_expression(body, env1) do
      result = Enum.reduce(Enum.reverse(param_types), body_type, &Types.t_arrow(&1, &2))
      {:ok, result, s1, env2}
    end
  end

  def infer_expression(%Ast.FunctionCall{function: fun, arguments: args}, env) do
    with {:ok, fun_t, s1, env1} <- infer_expression(fun, env) do
      {arg_results, {s_acc, env_acc, param_types}} =
        Enum.map_reduce(args, {s1, env1, []}, fn a, {s_prev, e_prev, pt} ->
          with {:ok, a_t, s_new, e_new} <- infer_expression(a, e_prev) do
            sub = Subst.compose(s_new, s_prev)
            {:ok, nil, {sub, e_new, [a_t | pt]}}
          end
        end)

      _ = arg_results
      # Create fresh result type variable
      {res_var, env2} = Env.fresh_var(env_acc)
      arrow_t = Enum.reduce(Enum.reverse(param_types), res_var, &Types.t_arrow(&1, &2))

      with {:ok, s2} <- Unify.unify(Subst.s_apply(s_acc, fun_t), Subst.s_apply(s_acc, arrow_t)) do
        {:ok, Subst.s_apply(s2, res_var), Subst.compose(s2, s_acc), env2}
      end
    end
  end

  def infer_expression(%Ast.BinaryOp{op: op, left: l, right: r}, env) do
    # For now assume numeric ops plus comparison
    with {:ok, lt, s1, env1} <- infer_expression(l, env),
         {:ok, rt, s2, env2} <- infer_expression(r, env1),
         {:ok, s3} <- Unify.unify(Subst.s_apply(s2, lt), Subst.s_apply(s2, rt)) do
      case op do
        op when op in ["+", "-", "*", "/"] ->
          {:ok, Types.t_int(), Subst.compose(s3, s2), env2}

        op when op in ["==", "!=", "<", "<=", ">", ">="] ->
          {:ok, Types.t_bool(), Subst.compose(s3, s2), env2}

        _ ->
          {:error, "Unknown operator #{op}"}
      end
    end
  end

  def infer_expression(%Ast.LetBinding{bindings: binds, body: body}, env) do
    {env1, sub_acc} =
      Enum.reduce(binds, {env, Subst.new()}, fn {name, expr}, {e, s} ->
        with {:ok, t, s1, e1} <- infer_expression(expr, e),
             s2 = Subst.compose(s1, s),
             e2 = generalize_bind(name, t, s2, e1) do
          {e2, s2}
        end
      end)

    with {:ok, bt, s_body, env2} <- infer_expression(body, env1) do
      {:ok, bt, Subst.compose(s_body, sub_acc), env2}
    end
  end

  def infer_expression(expr, _),
    do: {:error, "Expression form not yet supported: #{inspect(expr)}"}

  # ---------------------------------------------------------------------------
  # Function declarations ------------------------------------------------------
  # ---------------------------------------------------------------------------
  defp infer_function(
         %Ast.FunctionDeclaration{name: name, parameters: params, body: body, type_signature: ts},
         env
       ) do
    {param_types, env1} =
      Enum.map_reduce(params, env, fn p, e ->
        {v, e2} = Env.fresh_var(e, to_string(p.name || "p"))
        {v, Env.extend(e2, p.name, Scheme.new([], v))}
      end)

    with {:ok, body_t, s1, env2} <- infer_expression(body, env1) do
      fun_type = Enum.reduce(Enum.reverse(param_types), body_t, &Types.t_arrow(&1, &2))
      # If explicit signature, unify with inferred type
      case ts do
        nil ->
          {:ok, fun_type, s1, env2}

        %Ast.TypeSignature{} ->
          ts_type = convert_type(ts.type)

          with {:ok, s2} <- Unify.unify(Subst.s_apply(s1, fun_type), Subst.s_apply(s1, ts_type)) do
            {:ok, Subst.s_apply(s2, ts_type), Subst.compose(s2, s1), env2}
          end
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Helper utilities
  # ---------------------------------------------------------------------------
  defp generalize_bind(name, type, sub, env) do
    ftv_env = Types.free_type_vars_env(env)
    ftv_t = Types.free_type_vars(type) |> MapSet.new()

    qvars =
      MapSet.difference(ftv_t, ftv_env)
      |> Enum.map(fn id -> Types.TVar.new(id, :q) end)

    scheme = Scheme.new(qvars, type)
    Env.extend(env, name, scheme)
  end

  defp instantiate(%Scheme{vars: vars, type: t}, env) do
    {sub, _env} =
      Enum.reduce(vars, {Subst.new(), env}, fn v, {s, e} ->
        {fresh, e2} = Env.fresh_var(e)
        {Map.put(s, v.id, fresh), e2}
      end)

    Types.Subst.s_apply(sub, t)
  end

  # Convert parsed type AST into internal Types.t()
  defp convert_type(%Ast.Identifier{name: n}), do: Types.TCon.new(String.to_atom(n))

  defp convert_type(%Ast.FunctionCall{function: %Ast.Identifier{name: "[]"}, arguments: [el]}),
    do: Types.t_list(convert_type(el))

  defp convert_type(%Ast.Tuple{elements: els}), do: Types.t_tuple(Enum.map(els, &convert_type/1))

  defp convert_type(%Ast.BinaryOp{op: "->", left: a, right: b}),
    do: Types.t_arrow(convert_type(a), convert_type(b))

  defp convert_type(other), do: raise("Unsupported type syntax #{inspect(other)}")
end
