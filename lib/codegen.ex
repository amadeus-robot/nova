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

  defp compile_decl(%Ast.FunctionDeclaration{} = fun), do: compile_fun(fun)
  defp compile_decl(%Ast.ForeignImport{} = fi), do: gen_foreign(fi)
  # We ignore type/class declarations for now – they don't have Elixir code‑gen impact
  defp compile_decl(_), do: ""

  # ─────────────────────────────────────────────────────────────
  # Functions
  # ─────────────────────────────────────────────────────────────
  defp compile_fun(%Ast.FunctionDeclaration{name: name, parameters: ps, body: body}) do
    args = ps |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
    body_code = compile_expr(body) |> indent(2)

    """
    def #{name}(#{args}) do
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
  defp compile_expr(%Ast.Identifier{name: n}), do: n

  # Binary operation
  defp compile_expr(%Ast.BinaryOp{op: op, left: l, right: r}) do
    "#{compile_expr(l)} #{op} #{compile_expr(r)}"
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
      |> Enum.map(fn {name, value} -> "#{name} = #{compile_expr(value)}" end)
      |> Enum.join(";\n")

    """
    (fn ->
    #{indent(assigns, 4)}
      #{compile_expr(body)}
    end).()
    """
    |> String.trim()
  end

  # Case expression
  defp compile_expr(%Ast.CaseExpression{expression: e, cases: cs}) do
    clauses =
      cs
      |> Enum.map(fn %Ast.CaseClause{pattern: p, body: b} ->
        "#{compile_pattern(p)} -> #{compile_expr(b)}"
      end)
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

  # -- foreign import -------------------------------------------------
  defp gen_foreign(%Ast.ForeignImport{module: mod, function: fun, alias: name, type_signature: ts}) do
    arity = count_params(ts.type)
    args = for i <- 1..arity, do: "arg#{i}"

    """
    # foreign import #{mod}.#{fun}/#{arity}
    def #{name}(#{Enum.join(args, ", ")}) do
      apply(:'#{mod}', :'#{String.to_atom(fun)}', [#{Enum.join(args, ", ")}])
    end
    """
  end

  # ─────────────────────────────────────────────────────────────
  # Patterns – re‑using expression compiler for now
  # ─────────────────────────────────────────────────────────────
  defp compile_pattern(%Ast.Identifier{name: n}), do: n
  defp compile_pattern(%Ast.Literal{} = lit), do: compile_expr(lit)

  defp compile_pattern(%Ast.Tuple{elements: es}),
    do: "{" <> (es |> Enum.map(&compile_pattern/1) |> Enum.join(", ")) <> "}"

  defp compile_pattern(%Ast.List{elements: es}),
    do: "[" <> (es |> Enum.map(&compile_pattern/1) |> Enum.join(", ")) <> "]"

  defp compile_pattern(%Ast.FunctionCall{function: f, arguments: a}),
    do: compile_expr(%Ast.FunctionCall{function: f, arguments: a})

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

  # A Nova module name may arrive as an `Ast.Identifier` or a raw string.
  # Convert it to a valid Elixir module name string.
  defp to_elixir_modname(%Ast.Identifier{name: n}), do: n
  defp to_elixir_modname(n) when is_binary(n), do: n
end

