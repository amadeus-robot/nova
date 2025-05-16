defmodule Nova.Compiler.CodeGen do
  @moduledoc """
  First‑pass code generator that turns a parsed Nova `Ast.Module`
  into a valid Elixir module source string. All generated functions are
  `def` (public) so that the resulting code can be `Code.compile_string/1`
  ‑ed and invoked directly.

  This is intentionally minimal: it supports literals, identifiers,
  binary operators, function calls, `if`, basic `let` bindings, lambdas,
  case expressions, lists and tuples.  Patterns in function parameters
  and case clauses are emitted verbatim where possible.  Unsupported
  constructs will raise so that we can extend the generator incrementally
  as the Nova language grows.
  """

  alias Nova.Compiler.Ast
  alias Nova.Compiler.Types.TCon

  # ─────────────────────────────────────────────────────────────
  # Public API
  # ─────────────────────────────────────────────────────────────

  @doc """
  Compile a Nova AST module to an Elixir source string.
  """
  @spec compile(Ast.Module.t()) :: String.t()
  def compile(%Ast.Module{name: name_ast, declarations: decls}) do
    mod_name = to_elixir_modname(name_ast)

    body =
      decls
      |> Enum.map(&compile_decl/1)
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
  Compile a single Nova expression node to Elixir source.
  Useful for REPL evaluation or tests.
  """
  @spec compile_expression(any()) :: String.t()
  def compile_expression(expr), do: compile_expr(expr)

  # ─────────────────────────────────────────────────────────────
  # Declarations
  # ─────────────────────────────────────────────────────────────
  # ─────────────────────────────────────────────────────────────
  # Records  (type alias  Foo = { a :: Int, b :: String })
  # ─────────────────────────────────────────────────────────────
  defp compile_decl(%Ast.TypeAlias{
         name: rec_name,
         # ignore polymorphic vars for now
         type_vars: [],
         type: %Ast.RecordType{fields: flds}
       }) do
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
         %Ast.ImportDeclaration{
           module: %Ast.Identifier{name: "Effect.Console"},
           items: imps
         } = _imp
       ) do
    # was log requested?
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
      # only other symbols imported → ignore
      ""
    end
  end

  defp compile_decl(%Ast.FunctionDeclaration{} = fun), do: compile_fun(fun)
  defp compile_decl(%Ast.ForeignImport{} = fi), do: gen_foreign(fi)
  # We ignore type/class declarations for now – they don't have Elixir code‑gen impact
  defp compile_decl(_), do: ""

  # ─────────────────────────────────────────────────────────────
  # Functions
  # ─────────────────────────────────────────────────────────────
  defp compile_fun(%Ast.FunctionDeclaration{name: name, parameters: ps, body: body}) do
    sane_name = sanitize_name(name)
    args = ps |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
    body_code = compile_expr(body) |> indent(2)

    """
    def #{sane_name}(#{args}) do
    #{body_code}
    end
    """
    |> String.trim()
  end

  # ─────────────────────────────────────────────────────────────
  # Expressions
  # ─────────────────────────────────────────────────────────────
  # Literals
  defp compile_expr(%Ast.Literal{type: :number, value: v}), do: v
  defp compile_expr(%Ast.Literal{type: :string, value: v}), do: inspect(v)
  defp compile_expr(%Ast.Literal{type: :char, value: v}), do: "?#{v}"

  # Identifier
  defp compile_expr(%Ast.Identifier{name: n}), do: sanitize_name(n)

  defp compile_expr(%Ast.BinaryOp{op: "++", left: l, right: r}) do
    operator = if string_typed?(l) or string_typed?(r), do: "<>", else: "++"
    "#{compile_expr(l)} #{operator} #{compile_expr(r)}"
  end

  # Binary operation
  defp compile_expr(%Ast.BinaryOp{op: op, left: l, right: r}) do
    # wraps all operations in parentheses to keep precedence as the parser did assign em
    "(#{compile_expr(l)} #{op} #{compile_expr(r)})"
  end

  # Function call / application
  defp compile_expr(%Ast.FunctionCall{function: f, arguments: as}) do
    f_code =
      case f do
        %Ast.Identifier{} -> compile_expr(f)
        _ -> "(" <> compile_expr(f) <> ")"
      end

    arg_code = as |> Enum.map(&compile_expr/1) |> Enum.join(", ")
    "#{f_code}(#{arg_code})"
  end

  # If expression
  defp compile_expr(%Ast.IfExpression{condition: c, then_branch: t, else_branch: e}) do
    "if #{compile_expr(c)}, do: #{compile_expr(t)}, else: #{compile_expr(e)}"
  end

  # Lambda
  defp compile_expr(%Ast.Lambda{parameters: ps, body: b}) do
    params = ps |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
    "fn #{params} -> #{compile_expr(b)} end"
  end

  # Let – translated to an IIFE to preserve scope
  defp compile_expr(%Ast.LetBinding{bindings: bs, body: body}) do
    assigns =
      bs
      |> Enum.map(fn {name, value} -> "#{sanitize_name(name.name)} = #{compile_expr(value)}" end)
      |> Enum.join(";\n")

    """
    (fn ->
    #{indent(assigns, 4)}
      #{compile_expr(body)}
    end).()
    """
    |> String.trim()
  end

  defp compile_expr(%Ast.CaseExpression{expression: e, cases: cs}) do
    clauses =
      cs
      # ← use the helper
      |> Enum.map(&compile_case_clause/1)
      |> Enum.join("\n")

    """
    case #{compile_expr(e)} do
    #{indent(clauses, 2)}
    end
    """
    |> String.trim()
  end

  # Lists and tuples
  defp compile_expr(%Ast.List{elements: es}) do
    "[" <> (es |> Enum.map(&compile_expr/1) |> Enum.join(", ")) <> "]"
  end

  defp compile_expr(%Ast.Tuple{elements: es}) do
    "{" <> (es |> Enum.map(&compile_expr/1) |> Enum.join(", ")) <> "}"
  end

  # Fallback
  defp compile_expr(other), do: raise("Unsupported expression in codegen: #{inspect(other)}")

  defp count_params(%Ast.BinaryOp{op: "->", right: r}), do: 1 + count_params(r)
  defp count_params(_), do: 0

  # Detect whether an expression is statically typed as String (literal or inferred)
  defp string_typed?(%Ast.Literal{type: :string}), do: true

  # Any node carrying `inferred_type` metadata injected by the type‑checker
  defp string_typed?(%{inferred_type: %TCon{name: :String}}), do: true
  # safe fallback for other structs
  defp string_typed?(%{inferred_type: %{name: :String}}), do: true
  defp string_typed?(_), do: false

  # -- foreign import -------------------------------------------------
  defp gen_foreign(%Ast.ForeignImport{
         module: mod,
         function: fun,
         alias: name,
         type_signature: ts
       }) do
    arity = count_params(ts.type)
    args = for i <- 1..arity, do: "arg#{i}"
    sane_name = sanitize_name(name)

    """
    # foreign import #{mod}.#{fun}/#{arity}
    def #{sane_name}(#{Enum.join(args, ", ")}) do
      apply(:'#{mod}', :'#{String.to_atom(fun)}', [#{Enum.join(args, ", ")}])
    end
    """
  end

  # ─────────────────────────────────────────────────────────────
  # Patterns – re‑using expression compiler for now
  # ─────────────────────────────────────────────────────────────
  defp compile_pattern(%Ast.Identifier{name: n}), do: sanitize_name(n)
  defp compile_pattern(%Ast.Literal{} = lit), do: compile_expr(lit)
  defp compile_pattern(%Nova.Compiler.Ast.Wildcard{}), do: "_"

  defp compile_pattern(%Ast.Tuple{elements: es}),
    do: "{" <> (es |> Enum.map(&compile_pattern/1) |> Enum.join(", ")) <> "}"

  defp compile_pattern(%Ast.List{elements: es}),
    do: "[" <> (es |> Enum.map(&compile_pattern/1) |> Enum.join(", ")) <> "]"

  defp compile_pattern(%Ast.FunctionCall{
         function: %Nova.Compiler.Ast.Identifier{name: ":"},
         arguments: [a, b]
       }),
       do: "[ #{compile_pattern(a)} | #{compile_pattern(b)} ]"

  defp compile_pattern(%Ast.FunctionCall{function: f, arguments: a}),
    do: compile_expr(%Ast.FunctionCall{function: f, arguments: a})

  # NEW helper – compiles one case-clause, including an optional guard
  # ─────────────────────────────────────────────────────────────
  defp compile_case_clause(%Ast.CaseClause{pattern: p, guard: nil, body: b}) do
    "#{compile_pattern(p)} -> #{compile_expr(b)}"
  end

  defp compile_case_clause(%Ast.CaseClause{pattern: p, guard: g, body: b}) do
    "#{compile_pattern(p)} when #{compile_expr(g)} -> #{compile_expr(b)}"
  end

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

  # Replace prime (') with _prima to create valid Elixir identifiers
  defp sanitize_name(name) when is_binary(name), do: String.replace(name, "'", "_prima")

  # A Nova module name may arrive as an `Ast.Identifier` or a raw string.
  # Convert it to a valid Elixir module name string.
  defp to_elixir_modname(%Ast.Identifier{name: n}), do: n
  defp to_elixir_modname(n) when is_binary(n), do: n
end
