defmodule Nova.Compiler.TypeError do
  @moduledoc "Compile‑time type errors raised by the name‑resolver and HM checker."
  defexception [:message]
end

# ────────────────────────────────────────────────────────────────────────────
#  Name‑Resolver Pass
# ────────────────────────────────────────────────────────────────────────────

defmodule Nova.Compiler.NameResolver do
  @moduledoc """
  Phase‑0 pass that walks type syntax *before* Hindley–Milner inference and
  makes sure every type constructor name refers to a valid definition in the
  environment or an upstream `InterfaceRegistry` layer.  It also treats *type
  variables* declared on the current declaration as always valid.
  """

  alias Nova.Compiler.{Ast, Types, InterfaceRegistry, TypeError}
  # do **not** alias Record; Elixir.Kernel provides one
  alias Types.{TCon, TVar}

  @type namespace :: atom
  @type env :: Types.Env.t()
  @type layer :: InterfaceRegistry.t()

  # ────────────────────────────────────────────────────────────────────────────
  #  Public API
  # ────────────────────────────────────────────────────────────────────────────

  @spec resolve_block([any], namespace, layer, env) :: {[any], env}
  def resolve_block(decls, ns, layer, env = %{}) do
    Enum.reduce(decls, {[], env}, fn decl, {rev, e = %{}} ->
      IO.inspect(decl)
      {:ok, e1 = %{}} = ensure_decl!(decl, ns, layer, e)
      {[decl | rev], e1}
    end)
    |> then(fn {rev, e2} -> {Enum.reverse(rev), e2} end)
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Declaration‑level dispatch
  # ────────────────────────────────────────────────────────────────────────────
  defp ensure_decl!(%Ast.TypeAlias{name: n, type_vars: vars, type: rhs} = d, ns, layer, env) do
    # ①   put a placeholder scheme in the env
    env1 = Types.Env.bind_type(env, String.to_atom(n), length(vars))
    # ②   now walk the RHS with the enriched environment
    ensure_type!(rhs, ns, layer, env1, d)
    {:ok, env1}
  end

  defp ensure_decl!(%Ast.DataType{type_vars: vars, constructors: ctors} = d, ns, layer, env) do
    %{} = env1 = Types.Env.bind_type(env, d.name, length(vars))

    Enum.each(ctors, fn %Ast.DataConstructor{fields: flds} ->
      Enum.each(flds, fn fld ->
        IO.inspect({fld, ns, env1 != nil})
        ensure_type!(fld, ns, layer, env1, d)
      end)
    end)

    {:ok, env1}
  end

  defp ensure_decl!(%Ast.FunctionDeclaration{type_signature: nil}, _ns, _layer, env),
    do: {:ok, env}

  defp ensure_decl!(
         %Ast.FunctionDeclaration{type_signature: %Ast.TypeSignature{type: t}} = d,
         ns,
         layer,
         env
       ) do
    ensure_type!(t, ns, layer, env, d)
  end

  defp ensure_decl!(_other, _ns, _layer, env), do: {:ok, env}

  # ────────────────────────────────────────────────────────────────────────────
  #  Type‑syntax walk
  # ────────────────────────────────────────────────────────────────────────────
  defp ensure_type!(node, ns, _layer, env, _ctx) when env == nil do
    raise {WTF, node, ns}
  end

  # TVar nodes are always valid.
  defp ensure_type!(%TVar{}, _ns, _layer, _env, _ctx), do: :ok

  # Raw identifier → could be a type‑variable or a constructor.
  defp ensure_type!(%Ast.Identifier{name: n}, ns, layer, env, ctx) do
    ensure_ident!(n, ns, layer, env, ctx)
  end

  # Bare strings/atoms – same treatment.
  defp ensure_type!(name, ns, layer, env, ctx) when is_binary(name) or is_atom(name) do
    ensure_ident!(name, ns, layer, env, ctx)
  end

  # Type-application written in prefix form, e.g.  `Array String Int`
  # The parser encodes this as left-associative FunctionCall nodes, so we
  # flatten the chain and turn it into a single `TCon`.
  defp ensure_type!(%Ast.FunctionCall{} = call, ns, layer, env, ctx) do
    {head, args} = flatten_type_app(call)

    case head do
      %Ast.Identifier{name: name} ->
        # Re-use the existing TCon path (includes arity check, recursion, etc.)
        ensure_type!(%TCon{name: String.to_atom(name), args: args}, ns, layer, env, ctx)

      other ->
        raise TypeError,
          message:
            "Unsupported type application with head #{inspect(other)} in " <>
              format_ctx(ctx)
    end
  end

  # `TCon` – named type constructor with args.
  defp ensure_type!(%TCon{name: n, args: args} = con, ns, layer, env, ctx) do
    case resolve_tcon(n, ns, layer, env) do
      {:ok, ar} ->
        arity_check!(con, ar, ctx)
        Enum.each(args, &ensure_type!(&1, ns, layer, env, ctx))
        :ok

      :error ->
        raise TypeError, message: "Unknown type constructor \"#{n}\" in #{format_ctx(ctx)}"
    end
  end

  # Tuple, arrow, record containers.
  defp ensure_type!({:tuple, elems}, ns, layer, env, ctx) do
    Enum.each(elems, &ensure_type!(&1, ns, layer, env, ctx))
    :here
  end

  defp ensure_type!({:arrow, a, b}, ns, layer, env, ctx) do
    ensure_type!(a, ns, layer, env, ctx)
    ensure_type!(b, ns, layer, env, ctx)
  end

  defp ensure_type!(%Types.Record{fields: flds, row: tail}, ns, layer, env, ctx) do
    Enum.each(flds, fn {_lbl, t} -> ensure_type!(t, ns, layer, env, ctx) end)
    if tail, do: ensure_type!(tail, ns, layer, env, ctx)
  end

  # Record literal in type syntax, e.g.
  #   { name :: String, typeSignature :: TypeSignature | r }
  defp ensure_type!(%Ast.RecordType{fields: flds, row: tail}, ns, layer, env, ctx) do
    # validate each field type
    Enum.each(flds, fn {_lbl, t} -> ensure_type!(t, ns, layer, env, ctx) end)
    # row-variable (for open records) can be nil or an identifier / TVar
    if tail, do: ensure_type!(tail, ns, layer, env, ctx)
    :ok
  end

  # Catch‑all.
  defp ensure_type!(other, _ns, _layer, _env, _ctx) do
    raise "ensure_type!: unhandled node #{inspect(other)}"
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Identifier handling
  # ────────────────────────────────────────────────────────────────────────────
  defp ensure_ident!(name, _ns, _layer, _env, _ctx) when name in ["String", "Bool", "Int"] do
    :ok
  end

  defp ensure_ident!(name, ns, layer, env, ctx) do
    tv? =
      case ctx do
        %{type_vars: vars} when is_list(vars) -> Enum.member?(vars, to_string(name))
        _ -> false
      end

    if tv? do
      :ok
    else
      ensure_type!(%TCon{name: String.to_atom(name), args: []}, ns, layer, env, ctx)
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Helpers
  # ────────────────────────────────────────────────────────────────────────────

  defp resolve_tcon(name, ns, _layer, env) when env != nil do
    if Types.Env.type_defined?(env, name) do
      {:ok, Types.Env.arity(env, name)}
    else
      case InterfaceRegistry.get_scheme(env.registry_layer, ns, name) do
        %Types.Scheme{vars: vars} -> {:ok, length(vars)}
        :error -> :error
      end
    end
  end

  defp arity_check!(%TCon{args: a}, arity, _ctx) when length(a) == arity, do: :ok

  defp arity_check!(%TCon{name: n, args: a}, arity, ctx) do
    raise TypeError,
      message:
        "Kind mismatch: #{n} expects #{arity} type arg(s) but got #{length(a)} in #{format_ctx(ctx)}"
  end

  defp format_ctx(%{name: nm}), do: "\"#{nm}\""
  defp format_ctx(_), do: "declaration"

  defp flatten_type_app(%Ast.FunctionCall{function: fn_node, arguments: arg_nodes}) do
    {head, prev_args} = flatten_type_app(fn_node)
    {head, prev_args ++ arg_nodes}
  end

  defp flatten_type_app(node), do: {node, []}
end
