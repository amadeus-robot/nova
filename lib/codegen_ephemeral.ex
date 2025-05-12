defmodule Nova.Compiler.CodeGenEphem do
  alias Nova.Compiler.Ast
  alias Nova.Compiler.Types.TCon

  # ─────────────────────────────────────────────────────────────
  # Public API – ephemeral variant (for tests / REPL)
  # ─────────────────────────────────────────────────────────────

  @doc """
  Compile a Nova module into a *single* Elixir snippet where every
  top-level declaration is an *anonymous* function or value assignment.
  Nothing is wrapped in a `defmodule`, so the result can be evaluated
  with `Code.eval_string/1` inside tests without leaking globals.

      iex> {:ok, ast, _} = Parser.parse_module(tokens)
      iex> Code.eval_string(CodeGen.compile_ephemeral(ast))
  """
  @spec compile_ephemeral(Ast.Module.t()) :: String.t()
  def compile_ephemeral(%Ast.Module{name: name_ast, declarations: decls}) do
    header = "# module " <> to_elixir_modname(name_ast)

    body =
      decls
      |> Enum.map(&compile_decl_ephemeral/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.join("\n")

    [header, body] |> Enum.join("\n")
  end

  # ─────────────────────────────────────────────────────────────
  # Declarations  (ephemeral)
  # ─────────────────────────────────────────────────────────────

  defp compile_decl_ephemeral(%Ast.FunctionDeclaration{name: n, parameters: ps, body: b}) do
    args = ps |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
    fn_kw = if args == "", do: "fn ->", else: "fn #{args} ->"
    "#{n} = #{fn_kw} #{compile_expr_ephemeral(b)} end"
  end

  # Optionally expose foreign imports as anonymous wrappers too.
  defp compile_decl_ephemeral(%Ast.ForeignImport{
         module: m,
         function: f,
         alias: n,
         type_signature: ts
       }) do
    arity = count_params(ts.type)
    args = Enum.map_join(1..arity, ", ", &"arg#{&1}")
    "#{n} = fn #{args} -> apply(:'#{m}', :'#{f}', [#{args}]) end"
  end

  defp compile_decl_ephemeral(_), do: ""

  # ─────────────────────────────────────────────────────────────
  # Expressions  (ephemeral) — re-use normal clauses except calls
  # ─────────────────────────────────────────────────────────────

  # Call through a *variable* function:  swap.({1, 2})
  defp compile_expr_ephemeral(%Ast.FunctionCall{
         function: %Ast.Identifier{name: n},
         arguments: as
       }) do
    args = as |> Enum.map(&compile_expr_ephemeral/1) |> Enum.join(", ")
    "#{n}.(#{args})"
  end

  # Delegate everything else to the “normal” generator
  defp compile_expr_ephemeral(%Ast.FunctionCall{} = call), do: compile_expr(call)
  defp compile_expr_ephemeral(node), do: compile_expr(node)

  # …nothing below here changes …

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
      |> Enum.map(fn {name, value} -> "#{name.name} = #{compile_expr(value)}" end)
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
