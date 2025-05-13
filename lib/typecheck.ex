defmodule Nova.Compiler.TypeChecker do
  @moduledoc """
  Extended Hindley–Milner type‑checker for *Nova* following the design memo
  dated 13 May 2025 (Algorithm W + layered InterfaceRegistry).

  ── Public API ───────────────────────────────────────────────────────────────

  * `check_block/4` – infer a batch of declarations in *namespace* `ns`,
    writing all inferred interfaces into the given *registry layer* `reg`.
  * All other helpers are considered *internal* and **not part of the public
    surface**; they may change as the language grows.
  """

  alias Nova.Compiler.{Types, Unify, Ast, TypedAst}
  alias Nova.Compiler.Types.{Env, Scheme, Subst, TVar, TCon}
  alias Nova.InterfaceRegistry, as: IR

  # ---------------------------------------------------------------------------
  # Entry‑point – batch checker
  # ---------------------------------------------------------------------------

  @doc """
  Type‑check *decls* and append them to the current *registry layer* `reg`.
  Returns `{:ok, typed_decls, env}` on success or `{:error, reason}`.
  The caller decides whether to commit/discard the layer.
  """
  @spec check_block([Ast.t()], atom, IR.t(), Env.t()) ::
          {:ok, [TypedAst.t()], Env.t()} | {:error, String.t()}
  def check_block(decls, ns, reg, env \\ Env.empty()) do
    # NB: We do *not* store the namespace inside `Env`; keep it explicit.
    IO.inspect({decls, ns, env})

    Enum.reduce_while(decls, {:ok, [], env}, fn decl, {:ok, typed_acc, e} ->
      IO.puts("!check declaration")

      r =
        case check_declaration(decl, ns, reg, e) do
          {:ok, typed_decl, e2} -> {:cont, {:ok, [typed_decl | typed_acc], e2}}
          {:error, _} = err -> {:halt, err}
        end
    end)
    |> case do
      {:ok, typed_rev, e} -> {:ok, Enum.reverse(typed_rev), e}
      err -> err
    end
  end

  # ---------------------------------------------------------------------------
  # Declaration dispatcher
  # ---------------------------------------------------------------------------

  @spec check_declaration(Ast.t(), atom, IR.t(), Env.t()) ::
          {:ok, TypedAst.t(), Env.t()} | {:error, String.t()}
  defp check_declaration(%Ast.FunctionDeclaration{} = fun, ns, reg, env) do
    infer_function(fun, ns, reg, env)
  end

  defp check_declaration(%Ast.TypeSignature{} = sig, ns, reg, env) do
    record_type_signature(sig, ns, reg, env)
  end

  # TODO DataType, TypeAlias, Import, ForeignImport, etc.
  defp check_declaration(other, _ns, _reg, _env) do
    {:error, "Unsupported declaration: #{inspect(other.__struct__)}"}
  end

  # ---------------------------------------------------------------------------
  # ◈  Function inference  ◈
  # ---------------------------------------------------------------------------

  @spec infer_function(Ast.FunctionDeclaration.t(), atom, IR.t(), Env.t()) ::
          {:ok, TypedAst.FunctionDeclaration.t(), Env.t()} | {:error, String.t()}
  defp infer_function(
         %Ast.FunctionDeclaration{name: name, parameters: params, body: body, type_signature: sig} =
           _fun,
         ns,
         reg,
         env
       ) do
    {param_tys, env1} = allocate_params(params, env)

    with {:ok, typed_body, s_body, env2} <- infer_expr(body, ns, reg, env1),
         inferred_fun <- fold_arrow(Enum.reverse(param_tys), typed_body.type),
         {:ok, final_ty, s_total} <- maybe_unify_signature(sig, inferred_fun, s_body) do
      {typed_fun, env3} =
        generalise_bind_and_publish(name, final_ty, s_total, ns, reg, env2, %{
          params: params,
          param_tys: param_tys,
          body: typed_body,
          explicit?: not is_nil(sig)
        })

      {:ok, typed_fun, env3}
    end
  end

  # Allocate fresh type vars for λ‑params and extend the environment.
  @spec allocate_params([Ast.Identifier.t()], Env.t()) :: {[Types.t()], Env.t()}
  defp allocate_params(params, env) do
    Enum.map_reduce(params, env, fn %Ast.Identifier{name: id}, e ->
      {tv, e2} = Env.fresh_var(e, to_string(id))
      e3 = Env.extend(e2, id, Scheme.new([], tv))
      {tv, e3}
    end)
  end

  # Handle optional explicit signature.
  @spec maybe_unify_signature(Ast.TypeSignature.t() | nil, Types.t(), Subst.t()) ::
          {:ok, Types.t(), Subst.t()} | {:error, String.t()}
  defp maybe_unify_signature(nil, inferred, s_body), do: {:ok, inferred, s_body}

  defp maybe_unify_signature(%Ast.TypeSignature{type: ast_ty}, inferred, s_body) do
    explicit = convert_type(ast_ty)

    with {:ok, s_unify} <-
           Unify.unify(Subst.s_apply(s_body, inferred), Subst.s_apply(s_body, explicit)) do
      final_ty = Subst.s_apply(s_unify, explicit)
      {:ok, final_ty, Subst.compose(s_unify, s_body)}
    end
  end

  # Generalise, extend env, publish to registry; return typed declaration.
  @spec generalise_bind_and_publish(atom, Types.t(), Subst.t(), atom, IR.t(), Env.t(), map) ::
          {TypedAst.FunctionDeclaration.t(), Env.t()}
  defp generalise_bind_and_publish(name, type, subst, ns, reg, env, info) do
    env1 = apply_env(subst, env)
    scheme = generalise(type, env1)
    env2 = Env.extend(env1, name, scheme)

    typed = %TypedAst.FunctionDeclaration{
      name: name,
      parameters:
        Enum.zip(info.params, info.param_tys)
        |> Enum.map(fn {%Ast.Identifier{name: n}, t} ->
          %TypedAst.Identifier{name: n, type: t}
        end),
      body: info.body,
      type: type,
      explicit_signature?: info.explicit?
    }

    publish(reg, ns, name, scheme, typed)
    {typed, env2}
  end

  # ---------------------------------------------------------------------------
  # ◈  Expression inference (Algorithm W)  ◈
  # ---------------------------------------------------------------------------

  @type infer_result :: {:ok, TypedAst.t(), Subst.t(), Env.t()} | {:error, String.t()}

  @spec infer_expr(Ast.t(), atom, IR.t(), Env.t()) :: infer_result
  # Literals – limited set for now
  defp infer_expr(%Ast.Literal{type: :number, value: v}, _ns, _reg, env) do
    typed = %TypedAst.Literal{value: v, type: Types.t_int()}
    {:ok, typed, Subst.new(), env}
  end

  defp infer_expr(%Ast.Literal{type: :string, value: v}, _ns, _reg, env) do
    typed = %TypedAst.Literal{value: v, type: Types.t_string()}
    {:ok, typed, Subst.new(), env}
  end

  defp infer_expr(%Ast.Literal{type: :bool, value: v}, _ns, _reg, env) do
    typed = %TypedAst.Literal{value: v, type: Types.t_bool()}
    {:ok, typed, Subst.new(), env}
  end

  # Identifier lookup (env → registry)
  defp infer_expr(%Ast.Identifier{name: id}, ns, reg, env) do
    with {:ok, scheme} <- lookup(id, ns, reg, env),
         {t, env2} <- instantiate(scheme, env) do
      typed = %TypedAst.Identifier{name: id, type: t}
      {:ok, typed, Subst.new(), env2}
    else
      :error -> {:error, "Unbound identifier #{id}"}
    end
  end

  # Lambda
  defp infer_expr(%Ast.Lambda{parameters: params, body: body} = _lam, ns, reg, env) do
    {param_tys, env1} = allocate_params(params, env)

    with {:ok, body_node, s_body, env2} <- infer_expr(body, ns, reg, env1) do
      fun_t = fold_arrow(Enum.reverse(param_tys), body_node.type)

      typed = %TypedAst.Lambda{
        parameters:
          Enum.zip(params, param_tys)
          |> Enum.map(fn {%Ast.Identifier{name: n}, t} ->
            %TypedAst.Identifier{name: n, type: t}
          end),
        body: body_node,
        type: fun_t
      }

      {:ok, typed, s_body, env2}
    end
  end

  # Function call / application
  defp infer_expr(%Ast.FunctionCall{function: fn_ast, arguments: args}, ns, reg, env) do
    with {:ok, fn_node, s_fn, env1} <- infer_expr(fn_ast, ns, reg, env),
         {:ok, arg_nodes, s_args, env2, arg_tys} <- infer_args(args, ns, reg, env1),
         {res_tv, env3} <- Env.fresh_var(env2, "res"),
         want_t <- fold_arrow(Enum.reverse(arg_tys), res_tv),
         {:ok, s_unify} <-
           Unify.unify(Subst.s_apply(s_args, fn_node.type), Subst.s_apply(s_args, want_t)),
         s_total <- Subst.compose(s_unify, s_args),
         res_ty <- Subst.s_apply(s_total, res_tv) do
      typed = %TypedAst.FunctionCall{
        function: fn_node,
        arguments: arg_nodes,
        type: res_ty
      }

      {:ok, typed, s_total, env3}
    end
  end

  # If‑expression
  defp infer_expr(
         %Ast.IfExpression{condition: c, then_branch: t_b, else_branch: e_b},
         ns,
         reg,
         env
       ) do
    with {:ok, cond_node, s_c, env1} <- infer_expr(c, ns, reg, env),
         {:ok, nil} <- unify_bool(cond_node.type),
         {:ok, t_node, s_t, env2} <- infer_expr(t_b, ns, reg, env1),
         {:ok, e_node, s_e, env3} <- infer_expr(e_b, ns, reg, env2),
         {:ok, s_res} <-
           Unify.unify(Subst.s_apply(s_e, t_node.type), Subst.s_apply(s_e, e_node.type)),
         s_total <- Subst.compose(s_res, Subst.compose(s_e, Subst.compose(s_t, s_c))) do
      res_ty = Subst.s_apply(s_total, t_node.type)

      typed = %TypedAst.IfExpression{
        condition: cond_node,
        then_branch: t_node,
        else_branch: e_node,
        type: res_ty
      }

      {:ok, typed, s_total, env3}
    end
  end

  # Catch‑all
  defp infer_expr(expr, _ns, _reg, _env),
    do: {:error, "Expression not yet supported: #{expr.__struct__}"}

  # ---------------------------------------------------------------------------
  # ◈  Helpers  ◈
  # ---------------------------------------------------------------------------

  # Fold a right‑associative arrow chain: [a,b,c], res ⇒ a -> b -> c -> res
  @spec fold_arrow([Types.t()], Types.t()) :: Types.t()
  defp fold_arrow([], res), do: res
  defp fold_arrow([h | t], res), do: fold_arrow(t, Types.t_arrow(h, res))

  # Perform `generalise` after applying current substitution to `type`.
  @spec generalise(Types.t(), Env.t()) :: Scheme.t()
  defp generalise(type, env) do
    ftv_env = Types.free_type_vars_env(env)
    ftv_t = Types.free_type_vars(type)
    qvars = MapSet.difference(ftv_t, ftv_env) |> Enum.map(&TVar.new(&1, :q))
    Scheme.new(qvars, type)
  end

  # Apply substitution to every scheme in Env.
  @spec apply_env(Subst.t(), Env.t()) :: Env.t()
  defp apply_env(sub, %Env{m: m, counter: c} = _e) do
    m2 =
      Enum.into(m, %{}, fn {k, %Scheme{vars: q, type: t} = sch} ->
        {k, %Scheme{sch | type: Subst.s_apply(sub, t)}}
      end)

    %Env{m: m2, counter: c}
  end

  # Instantiate scheme → fresh copy.
  @spec instantiate(Scheme.t(), Env.t()) :: {Types.t(), Env.t()}
  defp instantiate(%Scheme{vars: q, type: t}, env) do
    {sub, env2} =
      Enum.map_reduce(q, env, fn %TVar{id: id}, e ->
        {fresh, e2} = Env.fresh_var(e, "i")
        {{id, fresh}, e2}
      end)

    sub_map = Map.new(sub)
    {Subst.s_apply(sub_map, t), env2}
  end

  # Lookup identifier first in env, then registry.
  @spec lookup(atom, atom, IR.t(), Env.t()) :: {:ok, Scheme.t()} | :error
  defp lookup(id, ns, reg, env) do
    case Env.lookup(env, id) do
      {:ok, sch} -> {:ok, sch}
      :error -> IR.get_scheme(reg, ns, id)
    end
  end

  # Publish an interface + typed ast to registry layer.
  @spec publish(IR.t(), atom, atom, Scheme.t(), TypedAst.t()) :: :ok
  defp publish(reg, ns, id, scheme, ast), do: IR.put(reg, ns, id, scheme, ast)

  # Ensure a type is Bool.
  defp unify_bool(t), do: Unify.unify(t, Types.t_bool())

  # Convert surface type syntax (AST) → internal representation.
  # *Very* incomplete – extend as needed.
  @spec convert_type(Ast.t()) :: Types.t()
  defp convert_type(%Ast.Identifier{name: :Int}), do: Types.t_int()
  defp convert_type(%Ast.Identifier{name: :String}), do: Types.t_string()
  defp convert_type(%Ast.Identifier{name: :Bool}), do: Types.t_bool()
  defp convert_type(other), do: raise("Type conversion TODO for #{inspect(other)}")

  # Infer argument list (left‑to‑right order; accumulates subst).
  @spec infer_args([Ast.t()], atom, IR.t(), Env.t()) ::
          {:ok, [TypedAst.t()], Subst.t(), Env.t(), [Types.t()]} | {:error, String.t()}
  defp infer_args(args, ns, reg, env) do
    Enum.reduce_while(args, {:ok, [], Subst.new(), env, []}, fn arg,
                                                                {status, acc_nodes, s_acc, e_acc,
                                                                 tys} ->
      case infer_expr(arg, ns, reg, e_acc) do
        {:ok, node, s_arg, e2} ->
          s_total = Subst.compose(s_arg, s_acc)
          {:cont, {:ok, [node | acc_nodes], s_total, e2, [Subst.s_apply(s_arg, node.type) | tys]}}

        {:error, _} = err ->
          {:halt, err}
      end
    end)
    |> case do
      {:ok, nodes_rev, s_final, env_final, tys_rev} ->
        {:ok, Enum.reverse(nodes_rev), s_final, env_final, Enum.reverse(tys_rev)}

      err ->
        err
    end
  end

  # Record a *stand‑alone* type signature (no implementation yet).
  # Simply adds it to Env so that later FunctionDecl can pick it up.
  @spec record_type_signature(Ast.TypeSignature.t(), atom, IR.t(), Env.t()) ::
          {:ok, TypedAst.TypeSignature.t(), Env.t()}
  defp record_type_signature(%Ast.TypeSignature{name: name, type: ast_ty} = sig, _ns, _reg, env) do
    scheme = Scheme.new([], convert_type(ast_ty))
    env2 = Env.extend(env, name, scheme)
    typed = %TypedAst.TypeSignature{sig | type: ast_ty}
    {:ok, typed, env2}
  end
end
