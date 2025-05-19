defmodule Nova.Compiler.CodeGen do
  @moduledoc """
  First‑pass code generator that turns a parsed Nova `Ast.Module`
  into a valid Elixir module source string.

  **NEW**: A compile‑time environment (`env`) is threaded through every
  generator helper so we can influence how code is emitted.  The env is a map
  with two keys:

    * `:namespace` – the Elixir module name we are currently generating.
    * `:generate_remote_calls` – when `true`, calls that would normally be
      emitted as local `foo()` become fully‑qualified `Namespace.foo()` so they
      still work when the code snippet is compiled/evaluated in isolation (e.g.
      in the REPL or tests).
  """

  alias Nova.Compiler.Ast
  alias Nova.Compiler.Types.TCon

  @type env :: %{
          namespace: String.t(),
          generate_remote_calls: boolean()
        }

  # ─────────────────────────────────────────────────────────────
  # Public API
  # ─────────────────────────────────────────────────────────────

  @doc """
  Compile a Nova `Ast.Module` into an Elixir source string.  Extra options:

    * `:generate_remote_calls` – propagate into `env.generate_remote_calls`.
  """
  @spec compile(Ast.Module.t(), keyword()) :: String.t()
  def compile(%Ast.Module{name: name_ast, declarations: decls}, opts \\ []) do
    mod_name = to_elixir_modname(name_ast)

    env = %{
      namespace: mod_name,
      generate_remote_calls: Keyword.get(opts, :generate_remote_calls, false)
    }

    body =
      decls
      |> Enum.map(&compile_decl(&1, env))
      |> Enum.reject(&(&1 == ""))
      |> Enum.join("\n\n")
      |> indent(2)

    """
    defmodule #{mod_name} do
    #{body}
    end
    """
    |> String.trim()
  end

  @doc """
  Compile a standalone Nova expression.  Useful for REPL evaluation or tests.
  Accepts the same options as `compile/2`.
  """
  @spec compile_expression(any(), keyword()) :: String.t()
  def compile_expression(expr, opts \\ []) do
    env = %{
      # If the caller does not care, give it a dummy namespace so remote‑call
      # generation still works deterministically.
      namespace: Keyword.get(opts, :namespace, "Tmp"),
      generate_remote_calls: Keyword.get(opts, :generate_remote_calls, false)
    }

    compile_expr(expr, env)
  end

  # ─────────────────────────────────────────────────────────────
  # Declarations
  # ─────────────────────────────────────────────────────────────
  defp compile_decl(
         %Ast.TypeAlias{
           name: rec_name,
           type_vars: [],
           type: %Ast.RecordType{fields: flds}
         },
         _env
       ) do
    mod_name = to_elixir_modname(rec_name)

    field_atoms =
      flds
      |> Enum.map(fn {label, _type} -> ":" <> sanitize_name(label) end)
      |> Enum.join(", ")

    """
    defmodule #{mod_name} do
      defstruct [#{field_atoms}]
    end
    """
    |> String.trim()
  end

  defp compile_decl(
         %Ast.ImportDeclaration{module: %Ast.Identifier{name: "Effect.Console"}, items: imps},
         _env
       ) do
    if Enum.any?(imps, fn
         %Ast.Identifier{name: "log"} -> true
         "log" -> true
         :log -> true
         _ -> false
       end) do
      """
      # Effect.Console.log – prints the value and threads it through
      def log(value) do
        IO.inspect(value, label: "log")
        value
      end
      """
      |> String.trim()
    else
      ""
    end
  end

  defp compile_decl(%Ast.FunctionDeclaration{} = fun, env), do: compile_fun(fun, env)
  defp compile_decl(%Ast.ForeignImport{} = fi, _env), do: gen_foreign(fi)
  defp compile_decl(_, _), do: ""

  # ─────────────────────────────────────────────────────────────
  # Functions
  # ─────────────────────────────────────────────────────────────
  defp compile_fun(%Ast.FunctionDeclaration{name: name, parameters: ps, body: body}, env) do
    sane_name = sanitize_name(name)
    args = ps |> Enum.map(&compile_pattern(&1, env)) |> Enum.join(", ")
    body_code = compile_expr(body, env) |> indent(2)

    """
    def #{sane_name}(#{args}) do
    #{body_code}
    end
    """
    |> String.trim()
  end

  # ─────────────────────────────────────────────────────────────
  # Expressions (env‑aware)
  # ─────────────────────────────────────────────────────────────
  # Wildcard & literals
  defp compile_expr(%Nova.Compiler.Ast.Wildcard{}, _env), do: "_"
  defp compile_expr(%Ast.Literal{type: :number, value: v}, _env), do: v
  defp compile_expr(%Ast.Literal{type: :string, value: v}, _env), do: inspect(v)
  defp compile_expr(%Ast.Literal{type: :char, value: v}, _env), do: "?#{v}"

  # Record literal
  defp compile_expr(%Ast.RecordLiteral{fields: fields}, env) do
    pairs =
      fields
      |> Enum.map(fn {label, expr} ->
        "#{sanitize_name(label)}: #{compile_expr(expr, env)}"
      end)
      |> Enum.join(", ")

    "%{#{pairs}}"
  end

  # Identifier
  defp compile_expr(%Ast.Identifier{name: n}, _env), do: sanitize_name(n)

  # Binary ops special‑cased
  defp compile_expr(%Ast.BinaryOp{op: "/=", left: l, right: r}, env) do
    "(#{compile_expr(l, env)} != #{compile_expr(r, env)})"
  end

  defp compile_expr(%Ast.BinaryOp{op: "++", left: l, right: r}, env) do
    operator = if string_typed?(l) or string_typed?(r), do: "<>", else: "++"
    "#{compile_expr(l, env)} #{operator} #{compile_expr(r, env)}"
  end

  defp compile_expr(%Ast.BinaryOp{op: op, left: l, right: r}, env) do
    "(#{compile_expr(l, env)} #{op} #{compile_expr(r, env)})"
  end

  # Function call / application – **env‑aware logic lives here**
  defp compile_expr(
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: <<cap::utf8, _::binary>> = cname},
           arguments: args
         },
         env
       )
       when cap >= ?A and cap <= ?Z do
    elems = Enum.map_join(args, ", ", &compile_expr(&1, env))
    "{#{sanitize_name(cname)}, #{elems}}"
  end

  defp compile_expr(%Ast.FunctionCall{function: f, arguments: as}, env) do
    f_code =
      case f do
        %Ast.Identifier{} ->
          base = compile_expr(f, env)

          if env.generate_remote_calls do
            "#{env.namespace}.#{base}"
          else
            base
          end

        _ ->
          "(" <> compile_expr(f, env) <> ")"
      end

    arg_code = as |> Enum.map(&compile_expr(&1, env)) |> Enum.join(", ")
    "#{f_code}(#{arg_code})"
  end

  # If expression
  defp compile_expr(%Ast.IfExpression{condition: c, then_branch: t, else_branch: e}, env) do
    "if #{compile_expr(c, env)}, do: #{compile_expr(t, env)}, else: #{compile_expr(e, env)}"
  end

  # Lambda
  defp compile_expr(%Ast.Lambda{parameters: ps, body: b}, env) do
    params = ps |> Enum.map(&compile_pattern(&1, env)) |> Enum.join(", ")
    "fn #{params} -> #{compile_expr(b, env)} end"
  end

  # Let binding (IIFE)
  defp compile_expr(%Ast.LetBinding{bindings: bs, body: body}, env) do
    assigns =
      bs
      |> Enum.map(&compile_let_binding(&1, env))
      |> Enum.join(";\n")

    """
    (fn ->
    #{indent(assigns, 4)}
      #{compile_expr(body, env)}
    end).()
    """
    |> String.trim()
  end

  # Let‑binding helpers ————————————————
  defp compile_let_binding(
         {%Ast.FunctionCall{
            function: %Ast.Identifier{name: <<f, _::binary>> = fname},
            arguments: args
          }, body},
         env
       )
       when f > 96 do
    params = Enum.map_join(args, ", ", &compile_pattern(&1, env))
    body_src = compile_expr(body, env) |> indent(4)

    """
    #{sanitize_name(fname)} = fn #{params} ->
    #{body_src}
    end
    """
    |> String.trim()
  end

  defp compile_let_binding({%Ast.Identifier{name: n}, value}, env) do
    "#{sanitize_name(n)} = #{compile_expr(value, env)}"
  end

  defp compile_let_binding({lhs, rhs}, env) do
    "#{compile_pattern(lhs, env)} = #{compile_expr(rhs, env)}"
  end

  defp compile_let_binding(
         {%Ast.FunctionCall{function: %Ast.Identifier{name: fname}, arguments: args}, value},
         env
       ) do
    params = args |> Enum.map(&compile_pattern(&1, env)) |> Enum.join(", ")

    """
    #{sanitize_name(fname)} = fn #{params} -> #{compile_expr(value, env)} end
    """
    |> String.trim()
  end

  # Case expression / clauses
  defp compile_expr(%Ast.CaseExpression{expression: e, cases: cs}, env) do
    clauses =
      cs
      |> Enum.map(&compile_case_clause(&1, env))
      |> Enum.join("\n")

    """
    case #{compile_expr(e, env)} do
    #{indent(clauses, 2)}
    end
    """
    |> String.trim()
  end

  # List & tuple literals
  defp compile_expr(%Ast.List{elements: es}, env) do
    "[" <> (es |> Enum.map(&compile_expr(&1, env)) |> Enum.join(", ")) <> "]"
  end

  defp compile_expr(%Ast.Tuple{elements: es}, env) do
    "{" <> (es |> Enum.map(&compile_expr(&1, env)) |> Enum.join(", ")) <> "}"
  end

  # Fallback
  defp compile_expr(other, _env),
    do: raise("Unsupported expression in codegen: #{inspect(other)}")

  # ─────────────────────────────────────────────────────────────
  # Case‑clause helper
  # ─────────────────────────────────────────────────────────────
  defp compile_case_clause(%Ast.CaseClause{pattern: p, guard: nil, body: b}, env),
    do: "#{compile_pattern(p, env)} -> #{compile_expr(b, env)}"

  defp compile_case_clause(%Ast.CaseClause{pattern: p, guard: g, body: b}, env),
    do: "#{compile_pattern(p, env)} when #{compile_expr(g, env)} -> #{compile_expr(b, env)}"

  # ─────────────────────────────────────────────────────────────
  # Patterns (env is only needed when they recurse into expressions)
  # ─────────────────────────────────────────────────────────────
  defp compile_pattern(%Ast.Identifier{name: n}, _env), do: sanitize_name(n)
  defp compile_pattern(%Ast.Literal{} = lit, env), do: compile_expr(lit, env)
  defp compile_pattern(%Nova.Compiler.Ast.Wildcard{}, _env), do: "_"

  defp compile_pattern(%Ast.Tuple{elements: es}, env),
    do: "{" <> (es |> Enum.map(&compile_pattern(&1, env)) |> Enum.join(", ")) <> "}"

  defp compile_pattern(%Ast.List{elements: es}, env),
    do: "[" <> (es |> Enum.map(&compile_pattern(&1, env)) |> Enum.join(", ")) <> "]"

  defp compile_pattern(
         %Ast.FunctionCall{function: %Ast.Identifier{name: "Tuple"}, arguments: args},
         env
       ) do
    "{" <> Enum.map_join(args, ", ", &compile_pattern(&1, env)) <> "}"
  end

  defp compile_pattern(
         %Ast.FunctionCall{function: %Nova.Compiler.Ast.Identifier{name: ":"}, arguments: [a, b]},
         env
       ),
       do: "[ #{compile_pattern(a, env)} | #{compile_pattern(b, env)} ]"

  defp compile_pattern(
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: <<cap::utf8, _::binary>> = cname},
           arguments: args
         },
         env
       )
       when cap >= ?A and cap <= ?Z do
    elems = Enum.map_join(args, ", ", &compile_pattern(&1, env))
    "{#{sanitize_name(cname)}, #{elems}}"
  end

  defp compile_pattern(%Ast.FunctionCall{function: f, arguments: a}, env),
    do: compile_expr(%Ast.FunctionCall{function: f, arguments: a}, env)

  # ─────────────────────────────────────────────────────────────
  # Helpers
  # ─────────────────────────────────────────────────────────────
  defp indent(str, n) do
    pad = String.duplicate(" ", n)

    str
    |> String.split("\n")
    |> Enum.map_join("\n", fn
      "" -> ""
      line -> pad <> line
    end)
  end

  defp sanitize_name("after"), do: "after_"
  defp sanitize_name(name) when is_binary(name), do: String.replace(name, "'", "_prima")

  defp to_elixir_modname(%Ast.Identifier{name: n}), do: n
  defp to_elixir_modname(n) when is_binary(n), do: n

  # String‑typed detection helpers – unchanged
  defp string_typed?(%Ast.Literal{type: :string}), do: true
  defp string_typed?(%{inferred_type: %TCon{name: :String}}), do: true
  defp string_typed?(%{inferred_type: %{name: :String}}), do: true
  defp string_typed?(_), do: false

  # Foreign import generator – unchanged
  defp count_params(%Nova.Compiler.Ast.ForAllType{vars: _, type: t}), do: count_params(t)
  defp count_params(%Ast.BinaryOp{op: "->", right: r}), do: 1 + count_params(r)
  defp count_params(_), do: 0

  def gen_foreign(%Ast.ForeignImport{module: mod, function: fun, alias: name, type_signature: ts}) do
    arity = count_params(ts.type)

    args =
      case arity do
        a when a < 1 -> []
        a when a == 1 -> ["arg1"]
        _ -> for i <- 1..arity, do: "arg#{i}"
      end

    sane_name = sanitize_name(name)

    """
    # foreign import #{mod}.#{fun}/#{arity}
    def #{sane_name}(#{Enum.join(args, ", ")}) do
      apply(:'#{mod}', :'#{String.to_atom(fun)}', [#{Enum.join(args, ", ")}])
    end
    """
  end
end
