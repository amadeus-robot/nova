defmodule Nova.Compiler.TypeChecker do
  @moduledoc """
  Extended Hindley–Milner type‑checker (algorithm W) for *Nova* that aims to
  cover the full set of language constructs currently accepted by the parser.

  This module supersedes earlier sketches – every expression or declaration
  emitted by the Nova parser **should** be handled here, or at least yield a
  descriptive error instead of a crash.  Unsupported constructs are clearly
  marked with TODOs so that tests can be added incrementally.
  """

  # ────────────────────────────────────────────────────────────────────────────
  #  Aliases & helpers
  # ────────────────────────────────────────────────────────────────────────────
  alias Nova.Compiler.{Ast, TypedAst, Types}
  alias Types.{Env, Scheme, Subst}
  alias Nova.Compiler.Unify

  # Convenience macro for fresh type‑variable allocation
  defmacrop fresh(env, hint \\ "t") do
    quote do
      Env.fresh_var(unquote(env), unquote(hint))
    end
  end

  @bool Types.t_bool()
  @int Types.t_int()
  @string Types.t_string()
  @char Types.t_char()

  # ────────────────────────────────────────────────────────────────────────────
  #  Public entry‑points
  # ────────────────────────────────────────────────────────────────────────────
  @spec check_module(Ast.Module.t(), Env.t()) ::
          {:ok, TypedAst.Module.t(), Env.t()} | {:error, term}
  def check_module(%Ast.Module{name: name, declarations: decls}, env \\ Env.empty()) do
    Enum.reduce_while(decls, {:ok, [], env}, fn decl, {:ok, typed_acc, e} ->
      case check_declaration(decl, e) do
        {:ok, typed_decl, e2} -> {:cont, {:ok, [typed_decl | typed_acc], e2}}
        err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, typed_list_rev, final_env} ->
        {:ok, %TypedAst.Module{name: name, declarations: Enum.reverse(typed_list_rev)}, final_env}

      other ->
        other
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Declarations
  # ────────────────────────────────────────────────────────────────────────────

  @spec check_declaration(Ast.t(), Env.t()) :: {:ok, TypedAst.t(), Env.t()} | {:error, term}

  # ── Functions ───────────────────────────────────────────────────────────────
  def check_declaration(%Ast.FunctionDeclaration{} = f, env) do
    with {:ok, typed_fun, _subst, env2} <- infer_function(f, env) do
      {:ok, typed_fun, env2}
    end
  end

  # ── Stand‑alone type signature ─────────────────────────────────────────────
  def check_declaration(%Ast.TypeSignature{} = sig, env) do
    type = convert_type(sig.type)
    env2 = Env.extend(env, sig.name, Scheme.new([], type))

    typed_sig = %TypedAst.TypeSignature{
      name: sig.name,
      type_vars: sig.type_vars,
      constraints: sig.constraints,
      type: type
    }

    {:ok, typed_sig, env2}
  end

  # ── Data declarations ──────────────────────────────────────────────────────
  def check_declaration(%Ast.DataType{} = dt, env), do: handle_data_type(dt, env)

  # ── Type aliases (transparent) ─────────────────────────────────────────────
  def check_declaration(%Ast.TypeAlias{name: name, type_vars: vars, type: aliased}, env) do
    aliased_t = convert_type(aliased)
    scheme = Scheme.new(Enum.map(vars, &Types.TVar.fresh/1), aliased_t)
    env2 = Env.extend(env, name, scheme)

    typed_alias = %TypedAst.TypeAlias{name: name, type_vars: vars, type: aliased_t}
    {:ok, typed_alias, env2}
  end

  # ── Type‑classes & instances – recorded but not checked yet ────────────────
  def check_declaration(%Ast.TypeClass{} = cls, env), do: {:ok, cls, env}
  def check_declaration(%Ast.TypeClassInstance{} = inst, env), do: {:ok, inst, env}

  # ── Foreign imports / Plain imports (ignored by TC) ────────────────────────
  def check_declaration(
        %Ast.ForeignImport{} = imp,
        env
      ) do
    {:ok, imp, env}
  end

  def check_declaration(%Ast.ImportDeclaration{} = imp, env), do: {:ok, imp, env}

  # ── Fallback ───────────────────────────────────────────────────────────────
  def check_declaration(other, _env), do: {:error, {"Unsupported declaration", other}}

  # ────────────────────────────────────────────────────────────────────────────
  #  Expression inference (algorithm W)
  # ────────────────────────────────────────────────────────────────────────────

  @spec infer_expression(Ast.t(), Env.t()) ::
          {:ok, TypedAst.t(), Subst.t(), Env.t()} | {:error, term}

  # Literals ──────────────────────────────────────────────────────────────────
  def infer_expression(%Ast.Literal{type: :number, value: v}, env),
    do: {:ok, %TypedAst.Literal{value: v, type: @int}, Subst.new(), env}

  def infer_expression(%Ast.Literal{type: :string, value: v}, env),
    do: {:ok, %TypedAst.Literal{value: v, type: @string}, Subst.new(), env}

  def infer_expression(%Ast.Literal{type: :char, value: v}, env),
    do: {:ok, %TypedAst.Literal{value: v, type: @char}, Subst.new(), env}

  def infer_expression(%Ast.Literal{type: :boolean, value: v}, env),
    do: {:ok, %TypedAst.Literal{value: v, type: @bool}, Subst.new(), env}

  # Identifiers ───────────────────────────────────────────────────────────────
  def infer_expression(%Ast.Identifier{name: n}, env) do
    case Env.lookup(env, n) do
      {:ok, scheme} ->
        type = instantiate(scheme, env)
        {:ok, %TypedAst.Identifier{name: n, type: type}, Subst.new(), env}

      :error ->
        {:error, "Unbound identifier #{n}"}
    end
  end

  # Lambda abstractions ───────────────────────────────────────────────────────
  def infer_expression(%Ast.Lambda{parameters: params, body: body}, env) do
    {param_types, env1} =
      Enum.map_reduce(params, env, fn p, e ->
        {tvar, e2} = fresh(e, to_string(p.name || "p"))
        {tvar, Env.extend(e2, p.name, Scheme.new([], tvar))}
      end)

    with {:ok, body_node, s1, env2} <- infer_expression(body, env1) do
      fun_type = Enum.reduce(Enum.reverse(param_types), body_node.type, &Types.t_arrow(&1, &2))

      node = %TypedAst.Lambda{parameters: params, body: body_node, type: fun_type}
      {:ok, node, s1, env2}
    end
  end

  # Function application (n‑ary) ──────────────────────────────────────────────
  def infer_expression(%Ast.FunctionCall{function: fun, arguments: args}, env) do
    with {:ok, fun_node, s1, env1} <- infer_expression(fun, env) do
      {arg_nodes, {s_acc, env_acc, param_types}} =
        Enum.map_reduce(args, {s1, env1, []}, fn a, {s_prev, e_prev, pt} ->
          with {:ok, a_node, s_new, e_new} <- infer_expression(a, e_prev) do
            sub = Subst.compose(s_new, s_prev)
            {a_node, {sub, e_new, [a_node.type | pt]}}
          end
        end)

      {res_type_var, env2} = fresh(env_acc)
      arrow_t = Enum.reduce(Enum.reverse(param_types), res_type_var, &Types.t_arrow(&1, &2))

      with {:ok, s2} <-
             Unify.unify(Subst.s_apply(s_acc, fun_node.type), Subst.s_apply(s_acc, arrow_t)) do
        res_type = Subst.s_apply(s2, res_type_var)

        node = %TypedAst.FunctionCall{
          function: fun_node,
          arguments: arg_nodes,
          type: res_type
        }

        {:ok, node, Subst.compose(s2, s_acc), env2}
      end
    end
  end

  # If / then / else ─────────────────────────────────────────────────────────
  def infer_expression(%Ast.IfExpression{condition: c, then_branch: t, else_branch: e}, env) do
    with {:ok, c_node, s1, env1} <- infer_expression(c, env),
         {:ok, s_bool} <- Unify.unify(c_node.type, @bool),
         s1 = Subst.compose(s_bool, s1),
         {:ok, t_node, s2, env2} <- infer_expression(t, env1),
         {:ok, e_node, s3, env3} <- infer_expression(e, env2),
         {:ok, s4} <- Unify.unify(Subst.s_apply(s3, t_node.type), Subst.s_apply(s3, e_node.type)) do
      final_sub = Enum.reduce([s4, s3, s2, s1], Subst.new(), &Subst.compose/2)
      res_type = Subst.s_apply(final_sub, t_node.type)

      node = %TypedAst.IfExpression{
        condition: c_node,
        then_branch: t_node,
        else_branch: e_node,
        type: res_type
      }

      {:ok, node, final_sub, env3}
    end
  end

  # Unary operators (currently only numeric negation) ────────────────────────
  def infer_expression(%Ast.UnaryOp{op: "-", value: v}, env) do
    with {:ok, v_node, s1, env1} <- infer_expression(v, env),
         {:ok, s2} <- Unify.unify(v_node.type, @int) do
      node = %TypedAst.UnaryOp{op: "-", value: v_node, type: @int}
      {:ok, node, Subst.compose(s2, s1), env1}
    end
  end

  # Lists ────────────────────────────────────────────────────────────────────
  def infer_expression(%Ast.List{elements: els}, env) do
    {typed_elems, {subst, env1, elem_type}} =
      Enum.map_reduce(els, {Subst.new(), env, nil}, fn el, {s_prev, e_prev, t_prev} ->
        with {:ok, el_node, s_el, e_el} <- infer_expression(el, e_prev) do
          sub = Subst.compose(s_el, s_prev)

          t_curr =
            cond do
              is_nil(t_prev) ->
                el_node.type

              true ->
                {:ok, s_unify} =
                  Unify.unify(Subst.s_apply(sub, t_prev), Subst.s_apply(sub, el_node.type))

                Subst.s_apply(s_unify, el_node.type)
            end

          {el_node, {sub, e_el, t_curr}}
        end
      end)

    list_type = Types.t_list(elem_type || elem_type_fresh(env1))
    {:ok, %TypedAst.List{elements: typed_elems, type: list_type}, subst, env1}
  end

  defp elem_type_fresh(env) do
    {t, _} = fresh(env, "a")
    t
  end

  # Tuples ───────────────────────────────────────────────────────────────────
  def infer_expression(%Ast.Tuple{elements: els}, env) do
    {typed, {subs, env1, types}} =
      Enum.map_reduce(els, {Subst.new(), env, []}, fn el, {s_prev, e_prev, ts} ->
        with {:ok, el_node, s_el, e_el} <- infer_expression(el, e_prev) do
          {el_node, {Subst.compose(s_el, s_prev), e_el, [el_node.type | ts]}}
        end
      end)

    tuple_type = Types.t_tuple(Enum.reverse(types))
    {:ok, %TypedAst.Tuple{elements: typed, type: tuple_type}, subs, env1}
  end

  # Record literals – shallow row‑polymorphic handling (closed) ───────────────
  def infer_expression(%Ast.RecordLiteral{fields: kv}, env) do
    {typed_fields, {subs, env1, f_types}} =
      Enum.map_reduce(kv, {Subst.new(), env, []}, fn {lbl, expr}, {s_prev, e_prev, acc} ->
        with {:ok, expr_node, s_e, e_e} <- infer_expression(expr, e_prev) do
          {{lbl, expr_node}, {Subst.compose(s_e, s_prev), e_e, [{lbl, expr_node.type} | acc]}}
        end
      end)

    rec_type = Types.t_record(Enum.reverse(f_types), :empty)
    {:ok, %TypedAst.RecordLiteral{fields: typed_fields, type: rec_type}, subs, env1}
  end

  # Let‑binding (non‑recursive) – already implemented earlier
  def infer_expression(%Ast.LetBinding{} = let, env), do: infer_let(let, env)

  # Catch‑all
  def infer_expression(expr, _), do: {:error, {"Unsupported expression", expr}}

  # ────────────────────────────────────────────────────────────────────────────
  #  Helpers split from huge clauses for clarity
  # ────────────────────────────────────────────────────────────────────────────

  # Let‑binding helper (non‑recursive)
  defp infer_let(%Ast.LetBinding{bindings: binds, body: body}, env) do
    {env1, sub_acc, typed_binds} =
      Enum.reduce(binds, {env, Subst.new(), []}, fn {name, expr}, {e, s, typed_list} ->
        with {:ok, expr_node, s1, e1} <- infer_expression(expr, e),
             s2 = Subst.compose(s1, s),
             e2 = generalize_bind(name, expr_node.type, s2, e1) do
          {e2, s2, [{name, expr_node} | typed_list]}
        end
      end)

    with {:ok, body_node, s_body, env2} <- infer_expression(body, env1) do
      node = %TypedAst.LetBinding{
        bindings: Enum.reverse(typed_binds),
        body: body_node,
        type: body_node.type
      }

      {:ok, node, Subst.compose(s_body, sub_acc), env2}
    end
  end

  # Data‑type helper (moved verbatim from earlier version)
  defp handle_data_type(dt, env) do
    %Ast.DataType{name: t_name, type_vars: vars_ast, constructors: ctors} = dt

    {vars, _ids} =
      Enum.map_reduce(vars_ast, elem(Env.fresh_var(env), 1).counter, fn var_name, id_acc ->
        {Types.TVar.new(id_acc, String.to_atom(var_name)), id_acc + 1}
      end)

    result_type = Types.TCon.new(String.to_atom(t_name), vars)

    {env_out, typed_ctors} =
      Enum.reduce(ctors, {env, []}, fn %Ast.DataConstructor{name: c_name, fields: fld_ast},
                                       {e_acc, typed_acc} ->
        {field_types, _} =
          Enum.map_reduce(fld_ast, e_acc, fn fa, _ -> {convert_type(fa), nil} end)

        cons_type = Enum.reduce(Enum.reverse(field_types), result_type, &Types.t_arrow(&1, &2))
        scheme = Scheme.new(vars, cons_type)
        e_next = Env.extend(e_acc, String.to_atom(c_name), scheme)
        typed_ctor = %TypedAst.DataConstructor{name: c_name, fields: field_types, type: cons_type}
        {e_next, [typed_ctor | typed_acc]}
      end)

    typed_dt = %TypedAst.DataType{
      name: t_name,
      type_vars: vars,
      constructors: Enum.reverse(typed_ctors)
    }

    {:ok, typed_dt, env_out}
  end

  # ---------------------------------------------------------------------------
  # Function declarations
  # ---------------------------------------------------------------------------
  defp infer_function(
         %Ast.FunctionDeclaration{
           name: name,
           parameters: params,
           body: body,
           type_signature: sig
         },
         env
       ) do
    {param_types, env1} =
      Enum.map_reduce(params, env, fn p, e ->
        {tvar, e2} = fresh(e, to_string(p.name || "p"))
        {tvar, Env.extend(e2, p.name, Scheme.new([], tvar))}
      end)

    with {:ok, body_node, s1, env2} <- infer_expression(body, env1) do
      fun_type_inferred =
        Enum.reduce(Enum.reverse(param_types), body_node.type, &Types.t_arrow(&1, &2))

      {final_type, s_final} =
        case sig do
          nil ->
            {fun_type_inferred, s1}

          %Ast.TypeSignature{} ->
            explicit = convert_type(sig.type)

            case Unify.unify(Subst.s_apply(s1, fun_type_inferred), Subst.s_apply(s1, explicit)) do
              {:ok, s2} -> {Subst.s_apply(s2, explicit), Subst.compose(s2, s1)}
              err -> throw(err)
            end
        end

      typed_fun = %TypedAst.FunctionDeclaration{
        name: name,
        parameters: params,
        body: body_node,
        type: final_type,
        explicit_signature?: sig != nil
      }

      env3 = generalize_bind(name, final_type, s_final, env2)
      {:ok, typed_fun, s_final, env3}
    end
  end

  # ---------------------------------------------------------------------------
  # Generalisation / instantiation helpers
  # ---------------------------------------------------------------------------

  defp generalize_bind(name, type, sub, env) do
    ftv_env = Types.free_type_vars_env(env)
    ftv_t = Types.free_type_vars(type)

    qvars =
      MapSet.difference(ftv_t, ftv_env)
      |> Enum.map(&Types.TVar.new(&1, :q))

    scheme = Scheme.new(qvars, type)
    Env.extend(env, name, scheme)
  end

  defp instantiate(%Scheme{vars: vars, type: t}, env) do
    {sub, _env} =
      Enum.reduce(vars, {Subst.new(), env}, fn v, {s, e} ->
        {fresh, e2} = fresh(e)
        {Map.put(s, v.id, fresh), e2}
      end)

    Types.Subst.s_apply(sub, t)
  end

  defp convert_type(ast), do: convert_type(ast, %{})

  # Main worker – threads `var_ctx` so we can resolve bound variables
  defp convert_type(%Ast.Identifier{name: n}, ctx) do
    case Map.get(ctx, n) do
      nil -> Types.TCon.new(String.to_atom(n))
      tvar -> tvar
    end
  end

  defp convert_type(%Ast.BinaryOp{op: "->", left: a, right: b}, ctx),
    do: Types.t_arrow(convert_type(a, ctx), convert_type(b, ctx))

  # Special list sugar – `[] a` emitted by the parser for list types
  defp convert_type(
         %Ast.FunctionCall{function: %Ast.Identifier{name: "[]"}, arguments: [el]},
         ctx
       ),
       do: Types.t_list(convert_type(el, ctx))

  # Generic **type application** – e.g. `List a`, `Either e a`, …
  defp convert_type(%Ast.FunctionCall{function: fun_ast, arguments: arg_asts} = call, ctx) do
    case convert_type(fun_ast, ctx) do
      %Types.TCon{name: name, args: prev} ->
        args = Enum.map(arg_asts, &convert_type(&1, ctx))
        Types.TCon.new(name, prev ++ args)

      other ->
        raise("Cannot apply non‑constructor #{inspect(other)} in #{inspect(call)}")
    end
  end

  # Tuples
  defp convert_type(%Ast.Tuple{elements: els}, ctx),
    do: Types.t_tuple(Enum.map(els, &convert_type(&1, ctx)))

  # Anything else is a bug for now
  defp convert_type(other, _), do: raise("Unsupported type syntax #{inspect(other)}")
end
