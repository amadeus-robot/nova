defmodule Nova.Compiler.NameResolver do
  @moduledoc """
  Phase‑0 pass that **resolves every type constructor name** before HM
  inference.  It guarantees that each `TCon` refers to a real definition in
  the current environment or one of the parent `InterfaceRegistry` layers and
  that the arity (kind) matches.
  """

  alias Nova.Compiler.{Ast, Types, InterfaceRegistry}
  alias Types.{TCon, TVar}

  @type namespace :: atom
  @type env :: Types.Env.t()
  @type layer :: InterfaceRegistry.t()

  # ────────────────────────────────────────────────────────────────────────────
  #  Entry‑point
  # ────────────────────────────────────────────────────────────────────────────

  @spec resolve_block([any], namespace, layer, env) :: {[any], env}
  def resolve_block(decls, ns, layer, env) do
    Enum.reduce(decls, {[], env}, fn decl, {rev, e} ->
      IO.inspect decl
      {:ok, e1} = ensure_decl!(decl, ns, layer, e)
      {[decl | rev], e1}
    end)
    |> then(fn {rev, e2} -> {Enum.reverse(rev), e2} end)
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Declaration dispatch
  # ────────────────────────────────────────────────────────────────────────────

  defp ensure_decl!(%Ast.TypeAlias{type: rhs} = d, ns, layer, env) do
    ensure_type!(rhs, ns, layer, env, d)
  end

  defp ensure_decl!(%Ast.DataType{type_vars: vars, constructors: ctors} = d, ns, layer, env) do
    env1 = Types.Env.bind_type(env, d.name, length(vars))

    Enum.each(ctors, fn %Ast.DataConstructor{fields: flds} ->
      Enum.each(flds, &ensure_type!(&1, ns, layer, env1, d))
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

  # ▶ Type variable – always OK.
  defp ensure_type!(%TVar{}, _ns, _layer, _env, _ctx), do: :ok

  # ▶ Raw identifier coming directly from the parser – treat as zero‑arity TCon.
  defp ensure_type!(%Ast.Identifier{name: n}, ns, layer, env, ctx) do
    ensure_type!(%TCon{name: String.to_atom(n), args: []}, ns, layer, env, ctx)
  end

  # ▶ Convenience – sometimes the parser leaves bare strings/atoms.
  defp ensure_type!(name, ns, layer, env, ctx) when is_binary(name) or is_atom(name) do
    ensure_type!(%TCon{name: String.to_atom(name), args: []}, ns, layer, env, ctx)
  end

  # ▶ Named type constructor.
  defp ensure_type!(%TCon{name: n, args: args} = con, ns, layer, env, ctx) do
    case resolve_tcon(n, ns, layer, env) do
      {:ok, ar} ->
        arity_check!(con, ar, ctx)
        Enum.each(args, &ensure_type!(&1, ns, layer, env, ctx))
        :ok

      :error ->
        raise TypeError,
              "Unknown type constructor \"#{n}\" in #{format_ctx(ctx)}"
    end
  end

  # ▶ Structural containers.
  defp ensure_type!({:tuple, elems}, ns, layer, env, ctx) do
    Enum.each(elems, &ensure_type!(&1, ns, layer, env, ctx))
  end

  defp ensure_type!({:arrow, a, b}, ns, layer, env, ctx) do
    ensure_type!(a, ns, layer, env, ctx)
    ensure_type!(b, ns, layer, env, ctx)
  end

  defp ensure_type!(
         %Nova.Compiler.Types.Record{fields: flds, row: tail} = rec,
         ns,
         layer,
         env,
         ctx
       ) do
    Enum.each(flds, fn {_lbl, t} -> ensure_type!(t, ns, layer, env, ctx) end)
    if tail, do: ensure_type!(tail, ns, layer, env, ctx)
  end

  # ▶ Catch‑all.
  defp ensure_type!(other, _ns, _layer, _env, _ctx) do
    raise "ensure_type!: unhandled node #{inspect(other)}"
  end

  # ────────────────────────────────────────────────────────────────────────────
  #  Helpers
  # ────────────────────────────────────────────────────────────────────────────

  defp resolve_tcon(name, _ns, _layer, env) do
    if name in [:String, :Int, :Bool] do
      {:ok, 0}
    else
      if Types.Env.type_defined?(env, name) do
        {:ok, Types.Env.arity(env, name)}
      else
        case InterfaceRegistry.get_scheme(env.registry_layer, env.namespace, name) do
          %Types.Scheme{vars: vars} -> {:ok, length(vars)}
          :error -> :error
        end
      end
    end
  end

  defp arity_check!(%TCon{name: n, args: a}, arity, ctx) when length(a) == arity, do: :ok

  defp arity_check!(%TCon{name: n, args: a}, arity, ctx) do
    raise TypeError,
          "Kind mismatch: #{n} expects #{arity} type arg(s) but got #{length(a)} in #{format_ctx(ctx)}"
  end

  defp format_ctx(%{name: nm}), do: inspect(nm)
  defp format_ctx(_), do: "declaration"
end
