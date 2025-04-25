defmodule Nova.Compiler.CodeGenTest do
  use ExUnit.Case, async: false

  alias Nova.Compiler.Ast
  alias Nova.Compiler.CodeGen

  # ------------------------------------------------------------------
  # Helpers to build simple AST nodes quickly inside each test
  # ------------------------------------------------------------------
  defp lit_num(n), do: %Ast.Literal{type: :number, value: Integer.to_string(n)}
  defp lit_str(s), do: %Ast.Literal{type: :string, value: s}
  defp id(name), do: %Ast.Identifier{name: name}

  # ------------------------------------------------------------------
  # Positive ‑ supported constructs
  # ------------------------------------------------------------------
  describe "compile/1 generates valid Elixir source" do
    test "simple arithmetic function" do
      ast = %Ast.Module{
        name: "Demo.Simple",
        declarations: [
          %Ast.FunctionDeclaration{
            name: "add",
            parameters: [id("a"), id("b")],
            body: %Ast.BinaryOp{op: "+", left: id("a"), right: id("b")}
          }
        ]
      }

      src = CodeGen.compile(ast)

      assert src =~ "defmodule Demo.Simple do"
      assert src =~ "def add(a, b) do"
      assert src =~ "a + b"

      [{Demo.Simple, _}] = Code.compile_string(src)
      assert Demo.Simple.add(2, 3) == 5
    end

    test "let‑binding round‑trip" do
      body = %Ast.LetBinding{
        bindings: [{"x", lit_num(1)}, {"y", lit_num(2)}],
        body: %Ast.BinaryOp{op: "+", left: id("x"), right: id("y")}
      }

      ast = %Ast.Module{
        name: "Demo.Let",
        declarations: [
          %Ast.FunctionDeclaration{name: "sum", parameters: [], body: body}
        ]
      }

      src = CodeGen.compile(ast)
      [{Demo.Let, _}] = Code.compile_string(src)
      assert Demo.Let.sum() == 3
    end

    test "case expression" do
      cases = [
        %Ast.CaseClause{pattern: lit_num(0), body: lit_str("zero")},
        %Ast.CaseClause{pattern: id("n"), body: lit_str("other")}
      ]

      fun = %Ast.FunctionDeclaration{
        name: "classify",
        parameters: [id("n")],
        body: %Ast.CaseExpression{expression: id("n"), cases: cases}
      }

      ast = %Ast.Module{name: "Demo.Case", declarations: [fun]}
      src = CodeGen.compile(ast)
      [{Demo.Case, _}] = Code.compile_string(src)
      assert Demo.Case.classify(0) == "zero"
      assert Demo.Case.classify(42) == "other"
    end

    test "lambda + lists + tuples inside expression compiler" do
      lam = %Ast.Lambda{
        parameters: [id("x")],
        body: %Ast.BinaryOp{op: "+", left: id("x"), right: lit_num(1)}
      }

      list = %Ast.List{elements: [lit_num(1), lit_num(2)]}
      tuple = %Ast.Tuple{elements: [lit_str("a"), lit_str("b")]}

      for expr <- [lam, list, tuple] do
        src = CodeGen.compile_expression(expr)
        {value, _} = Code.eval_string(src, [])

        case expr do
          %Ast.Lambda{} -> assert value.(5) == 6
          %Ast.List{} -> assert value == [1, 2]
          %Ast.Tuple{} -> assert value == {"a", "b"}
        end
      end
    end
  end

  # ------------------------------------------------------------------
  # Negative ‑ features not yet supported should raise
  # ------------------------------------------------------------------
  describe "unsupported constructs raise clearly" do
    test "unary operator" do
      ast = %Ast.UnaryOp{op: "-", value: lit_num(1)}

      assert_raise RuntimeError, ~r/Unsupported expression/, fn ->
        CodeGen.compile_expression(ast)
      end
    end

    test "do‑block" do
      ast = %Ast.DoBlock{expressions: []}

      assert_raise RuntimeError, ~r/Unsupported expression/, fn ->
        CodeGen.compile_expression(ast)
      end
    end

    test "list comprehension" do
      ast = %Ast.ListComprehension{expression: lit_num(1), generators: [], guards: []}

      assert_raise RuntimeError, ~r/Unsupported expression/, fn ->
        CodeGen.compile_expression(ast)
      end
    end
  end

  # ------------------------------------------------------------------
  # Internal helpers compile_expression should never output trailing ws
  # ------------------------------------------------------------------
  test "compile_expression output is trimmed" do
    src = CodeGen.compile_expression(lit_num(7))
    assert src == "7"
  end
end
