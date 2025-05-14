defmodule Nova.Compiler.TypeChecker do
  @moduledoc """
  Extended Hindley–Milner type-checker for *Nova* following the design memo
  dated 13 May 2025 (Algorithm W + layered InterfaceRegistry).

  ── Public API ───────────────────────────────────────────────────────────────

  * `check_block/4` – infer a batch of declarations in *namespace* `ns`,
    writing all inferred interfaces into the given *registry layer* `reg`.
  * All other helpers are considered *internal* and **not part of the public
    surface**; they may change as the language grows.
  """

  alias Nova.Compiler.{Types, Unify, Ast, TypedAst}
  alias Nova.Compiler.Types.{Env, Scheme, Subst, TVar, TCon}
  alias Nova.Compiler.InterfaceRegistry, as: IR

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
    Enum.reduce_while(decls, {:ok, [], env}, fn decl, {:ok, acc, e} ->
      case check_declaration(decl, ns, reg, e) do
        {:ok, typed_decl, e2} -> {:cont, {:ok, [typed_decl | acc], e2}}
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
          {:ok, TypedAst.t() | Ast.t(), Env.t()} | {:error, String.t()}
  defp check_declaration(%Ast.FunctionDeclaration{} = fun, ns, reg, env),
    do: infer_function(fun, ns, reg, env)

  defp check_declaration(%Ast.TypeSignature{} = sig, ns, reg, env),
    do: record_type_signature(sig, ns, reg, env)

  defp check_declaration(%Ast.ImportDeclaration{} = imp, ns, reg, env),
    do: process_import(imp, ns, reg, env)

  defp check_declaration(%Ast.DataType{} = dt, ns, reg, env),
    do: define_data_type(dt, ns, reg, env)

  defp check_declaration(%Ast.TypeAlias{} = ta, ns, reg, env),
    do: define_type_alias(ta, ns, reg, env)

  defp check_declaration(%Ast.ForeignImport{} = fi, ns, reg, env),
    do: define_foreign(fi, ns, reg, env)

  defp check_declaration(other, _ns, _reg, _env),
    do: {:error, "Unsupported declaration #{inspect(other.__struct__)}"}

  # ---------------------------------------------------------------------------
  # ◈  Import handling  ◈
  # ---------------------------------------------------------------------------

  @spec process_import(Ast.ImportDeclaration.t(), atom, IR.t(), Env.t()) ::
          {:ok, Ast.ImportDeclaration.t(), Env.t()} | {:error, String.t()}
  defp process_import(
         %Ast.ImportDeclaration{
           module: %Ast.Identifier{name: mod_str},
           items: items,
           hiding?: hiding?,
           alias: _al
         } = imp,
         _ns,
         reg,
         env
       ) do
    imported_ns = String.to_atom(mod_str)

    with {:ok, exports} <- IR.list_exports(reg, imported_ns) do
      selected =
        cond do
          items == [] and hiding? == false -> exports
          hiding? == true -> Map.drop(exports, Enum.map(items, &import_key/1))
          true -> Map.take(exports, Enum.map(items, &import_key/1))
        end

      env2 = Enum.reduce(selected, env, fn {id, sch}, e -> Env.extend(e, id, sch) end)
      {:ok, imp, env2}
    else
      :error -> {:error, "Unknown module #{mod_str}"}
      {:error, reason} -> {:error, reason}
    end
  end

  defp import_key(item) when is_binary(item), do: String.to_atom(item)
  defp import_key({name, _}), do: String.to_atom(name)
  defp import_key(other), do: other

  # ---------------------------------------------------------------------------
  # ◈  Function inference  ◈
  # ---------------------------------------------------------------------------

  @spec infer_function(Ast.FunctionDeclaration.t(), atom, IR.t(), Env.t()) ::
          {:ok, TypedAst.FunctionDeclaration.t(), Env.t()} | {:error, String.t()}
  defp infer_function(
         %Ast.FunctionDeclaration{
           name: name,
           parameters: params,
           body: body,
           type_signature: sig
         },
         ns,
         reg,
         env
       ) do
    {param_tys, env1} = allocate_params(params, env)

    with {:ok, body_node, s_body, env2} <- infer_expr(body, ns, reg, env1),
         inferred_fun <- fold_arrow(Enum.reverse(param_tys), body_node.type),
         {:ok, final_ty, s_total} <- maybe_unify_signature(sig, inferred_fun, s_body),
         {typed_fun, env3} <-
           generalise_bind_and_publish(name, final_ty, s_total, ns, reg, env2, %{
             params: params,
             param_tys: param_tys,
             body: body_node,
             explicit?: not is_nil(sig)
           }) do
      {:ok, typed_fun, env3}
    end
  end

  # ---------------------------------------------------------------------------
  # fresh vars for λ params
  # ---------------------------------------------------------------------------

  @spec allocate_params([Ast.Identifier.t()], Env.t()) :: {[Types.t()], Env.t()}
  defp allocate_params(params, env) do
    Enum.map_reduce(params, env, fn %Ast.Identifier{name: id}, e ->
      {tv, e2} = Env.fresh_var(e, to_string(id))
      e3 = Env.extend(e2, id, Scheme.new([], tv))
      {tv, e3}
    end)
  end

  # ---------------------------------------------------------------------------
  # explicit signature unification (optional)
  # ---------------------------------------------------------------------------

  @spec maybe_unify_signature(Ast.TypeSignature.t() | nil, Types.t(), Subst.t()) ::
          {:ok, Types.t(), Subst.t()} | {:error, String.t()}
  defp maybe_unify_signature(nil, inferred, s_body), do: {:ok, inferred, s_body}

  defp maybe_unify_signature(%Ast.TypeSignature{type: ast_ty}, inferred, s_body) do
    explicit = convert_type(ast_ty)

    with {:ok, s_unify} <-
           Unify.unify(Subst.s_apply(s_body, inferred), Subst.s_apply(s_body, explicit)) do
      {:ok, Subst.s_apply(s_unify, explicit), Subst.compose(s_unify, s_body)}
    end
  end

  # ---------------------------------------------------------------------------
  # bind Generalise & publish
  # ---------------------------------------------------------------------------

  @spec generalise_bind_and_publish(atom, Types.t(), Subst.t(), atom, IR.t(), Env.t(), map) ::
          {TypedAst.FunctionDeclaration.t(), Env.t()}
  defp generalise_bind_and_publish(name, type, sub, ns, reg, env, info) do
    env1 = apply_env(sub, env)
    scheme = generalise(type, env1)
    env2 = Env.extend(env1, name, scheme)

    params_typed =
      Enum.zip(info.params, info.param_tys)
      |> Enum.map(fn {%Ast.Identifier{name: n}, t} -> %TypedAst.Identifier{name: n, type: t} end)

    typed = %TypedAst.FunctionDeclaration{
      name: name,
      parameters: params_typed,
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
  # literals
  defp infer_expr(%Ast.Literal{type: :number, value: v}, _ns, _reg, env),
    do: {:ok, %TypedAst.Literal{value: v, type: Types.t_int()}, Subst.new(), env}

  defp infer_expr(%Ast.Literal{type: :string, value: v}, _ns, _reg, env),
    do: {:ok, %TypedAst.Literal{value: v, type: Types.t_string()}, Subst.new(), env}

  defp infer_expr(%Ast.Literal{type: :bool, value: v}, _ns, _reg, env),
    do: {:ok, %TypedAst.Literal{value: v, type: Types.t_bool()}, Subst.new(), env}

  # identifier lookup
  defp infer_expr(%Ast.Identifier{name: id}, ns, reg, env) do
    with {:ok, sch} <- lookup(id, ns, reg, env),
         {t, env2} <- instantiate(sch, env) do
      {:ok, %TypedAst.Identifier{name: id, type: t}, Subst.new(), env2}
    else
      :error -> {:error, "Unbound identifier #{id}"}
    end
  end

  # lambda
  defp infer_expr(%Ast.Lambda{parameters: params, body: body}, ns, reg, env) do
    {param_tys, env1} = allocate_params(params, env)

    with {:ok, body_node, s_body, env2} <- infer_expr(body, ns, reg, env1) do
      fun_t = fold_arrow(Enum.reverse(param_tys), body_node.type)

      params_typed =
        Enum.zip(params, param_tys)
        |> Enum.map(fn {%Ast.Identifier{name: n}, t} ->
          %TypedAst.Identifier{name: n, type: t}
        end)

      typed = %TypedAst.Lambda{parameters: params_typed, body: body_node, type: fun_t}
      {:ok, typed, s_body, env2}
    end
  end

  # application
  defp infer_expr(%Ast.FunctionCall{function: fn_ast, arguments: args}, ns, reg, env) do
    with {:ok, fn_node, s_fn, env1} <- infer_expr(fn_ast, ns, reg, env),
         {:ok, arg_nodes, s_args, env2, arg_tys} <- infer_args(args, ns, reg, env1),
         {res_tv, env3} <- Env.fresh_var(env2, "res"),
         want_t <- fold_arrow(Enum.reverse(arg_tys), res_tv),
         {:ok, s_unify} <-
           Unify.unify(Subst.s_apply(s_args, fn_node.type), Subst.s_apply(s_args, want_t)),
         s_total <- Subst.compose(s_unify, s_args),
         res_ty <- Subst.s_apply(s_total, res_tv) do
      typed = %TypedAst.FunctionCall{function: fn_node, arguments: arg_nodes, type: res_ty}
      {:ok, typed, s_total, env3}
    end
  end

  # if
  defp infer_expr(
         %Ast.IfExpression{condition: c, then_branch: t_b, else_branch: e_b},
         ns,
         reg,
         env
       ) do
    with {:ok, cond_node, s_c, env1} <- infer_expr(c, ns, reg, env),
         {:ok, _} <- Unify.unify(cond_node.type, Types.t_bool()),
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

  # fallback
  defp infer_expr(expr, _ns, _reg, _env),
    do: {:error, "Expression not supported: #{expr.__struct__}"}

  # ---------------------------------------------------------------------------
  # ◈  Misc helpers  ◈
  # ---------------------------------------------------------------------------

  defp fold_arrow([], res), do: res
  defp fold_arrow([h | t], res), do: fold_arrow(t, Types.t_arrow(h, res))

  defp generalise(type, env) do
    ftv_env = Types.free_type_vars_env(env)
    ftv_t = Types.free_type_vars(type)
    qvars = MapSet.difference(ftv_t, ftv_env) |> Enum.map(&TVar.new(&1, :q))
    Scheme.new(qvars, type)
  end

  defp apply_env(sub, %Env{m: m, counter: c}) do
    m2 =
      Enum.into(m, %{}, fn {k, %Scheme{vars: q, type: t} = sch} ->
        {k, %Scheme{sch | type: Subst.s_apply(sub, t)}}
      end)

    %Env{m: m2, counter: c}
  end

  defp instantiate(%Scheme{vars: q, type: t}, env) do
    {sub_pairs, env2} =
      Enum.map_reduce(q, env, fn %TVar{id: id}, e ->
        {fresh, e2} = Env.fresh_var(e, "i")
        {{id, fresh}, e2}
      end)

    sub = Map.new(sub_pairs)
    {Subst.s_apply(sub, t), env2}
  end

  defp lookup(id, ns, reg, env) do
    case Env.lookup(env, id) do
      {:ok, sch} -> {:ok, sch}
      :error -> IR.get_scheme(reg, ns, id)
    end
  end

  defp publish(reg, ns, id, scheme, ast), do: IR.put(reg, ns, id, scheme, ast)

  @spec infer_args([Ast.t()], atom, IR.t(), Env.t()) ::
          {:ok, [TypedAst.t()], Subst.t(), Env.t(), [Types.t()]} | {:error, String.t()}
  defp infer_args(args, ns, reg, env) do
    Enum.reduce_while(args, {:ok, [], Subst.new(), env, []}, fn arg,
                                                                {:ok, acc_nodes, s_acc, e_acc,
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

  @spec record_type_signature(Ast.TypeSignature.t(), atom, IR.t(), Env.t()) ::
          {:ok, TypedAst.TypeSignature.t(), Env.t()}
  defp record_type_signature(%Ast.TypeSignature{name: name, type: ast_ty} = sig, _ns, _reg, env) do
    scheme = Scheme.new([], convert_type(ast_ty))
    env2 = Env.extend(env, name, scheme)
    typed = %TypedAst.TypeSignature{sig | type: ast_ty}
    {:ok, typed, env2}
  end

  # ───────────────────────────────────────────────────────────────────
  #  ◈  Type aliases  ◈
  # ───────────────────────────────────────────────────────────────────

  defp define_type_alias(
         %Ast.TypeAlias{name: name, type_vars: vars, type: rhs_ast} = ali,
         ns,
         reg,
         env
       ) do
    {tv_map, _env1} = alloc_type_vars(vars, env)
    alias_type = convert_type(rhs_ast, tv_map)

    scheme = Scheme.new(Map.values(tv_map), alias_type)
    publish(reg, ns, name, scheme, ali)

    {:ok, %TypedAst.TypeAlias{name: name, type_vars: vars, type: alias_type}, env}
  end

  # ───────────────────────────────────────────────────────────────────
  #  ◈  Foreign imports  ◈
  # ───────────────────────────────────────────────────────────────────

  defp define_foreign(
         %Ast.ForeignImport{alias: as?, function: fun, type_signature: sig} = decl,
         ns,
         reg,
         env
       ) do
    id = as? || fun
    ty = convert_type(sig.type)
    scheme = Scheme.new([], ty)

    publish(reg, ns, id, scheme, %TypedAst.ForeignImport{alias: as?, function: fun, type: ty})
    {:ok, decl, Env.extend(env, id, scheme)}
  end

  # ───────────────────────────────────────────────────────────────────
  #  ◈  Algebraic data-type handling  ◈
  # ───────────────────────────────────────────────────────────────────

  defp define_data_type(
         %Ast.DataType{name: name, type_vars: vars, constructors: ctors} = dt,
         ns,
         reg,
         env
       ) do
    {tv_map, env1} = alloc_type_vars(vars, env)

    # Build the fully-applied type constructor for the RHS of every ctor
    data_tcon =
      Types.TCon.new(String.to_atom(name), Map.values(tv_map))

    {typed_ctors, env2} =
      Enum.map_reduce(ctors, env1, fn %Ast.DataConstructor{name: c_name, fields: fs}, e ->
        f_types = Enum.map(fs, &convert_type(&1, tv_map))
        ctor_ty = fold_arrow(Enum.reverse(f_types), data_tcon)

        sch = Scheme.new(Map.values(tv_map), ctor_ty)

        publish(reg, ns, c_name, sch, %TypedAst.DataConstructor{
          name: c_name,
          fields: f_types,
          type: ctor_ty
        })

        {c_name, Env.extend(e, c_name, sch)}
      end)

    typed_dt = %TypedAst.DataType{
      name: name,
      type_vars: Map.values(tv_map),
      constructors: typed_ctors
    }

    {:ok, typed_dt, env2}
  end

  # ───────────────────────────────────────────────────────────────────
  #  Helpers for allocating & mapping bound type variables
  # ───────────────────────────────────────────────────────────────────

  defp alloc_type_vars(var_names, env) do
    Enum.map_reduce(var_names, env, fn v, e ->
      {tv, e2} = Env.fresh_var(e, v)
      {{v, tv}, e2}
    end)
    |> then(fn {pairs, e2} -> {Map.new(pairs), e2} end)
  end

  # ───────────────────────────────────────────────────────────────────
  #  Extended type-AST → Types.t() converter
  # ───────────────────────────────────────────────────────────────────

  defp convert_type(%Ast.Identifier{name: :Int}), do: Types.t_int()
  defp convert_type(%Ast.Identifier{name: :String}), do: Types.t_string()
  defp convert_type(%Ast.Identifier{name: :Bool}), do: Types.t_bool()
  defp convert_type(ast), do: convert_type(ast, %{})

  # Identifier → built-in | bound var | 0-ary constructor
  defp convert_type(%Ast.Identifier{name: n}, varmap) do
    cond do
      n == :Int -> Types.t_int()
      n == :String -> Types.t_string()
      n == :Bool -> Types.t_bool()
      Map.has_key?(varmap, n) -> Map.fetch!(varmap, n)
      # user-defined 0-ary type
      true -> Types.TCon.new(n, [])
    end
  end

  # Type application: e.g. `List Int` comes from the parser as
  # `%Ast.FunctionCall{function: List, arguments: [Int]}`.
  defp convert_type(%Ast.FunctionCall{function: f, arguments: args}, varmap) do
    %Types.TCon{name: ctor, args: old} = convert_type(f, varmap)
    new_args = Enum.map(args, &convert_type(&1, varmap))
    %Types.TCon{name: ctor, args: old ++ new_args}
  end

  defp convert_type(%Ast.RecordType{row: row} = rec, _) when row not in [nil, :empty] do
    raise "Row-polymorphic records are not implemented yet (#{inspect(rec)})"
  end

  defp convert_type(%Ast.RecordType{fields: fs}, varmap) do
    field_map =
      for {label, ast_t} <- fs, into: %{} do
        {label, convert_type(ast_t, varmap)}
      end

    %Types.Record{fields: field_map}
  end

  # Fallback – extend as the grammar grows
  defp convert_type(other, _map),
    do: raise("Unsupported type syntax: #{inspect(other)}")
end
