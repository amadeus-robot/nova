# record_parser_test.exs – exhaustive parser coverage for record‑related syntax
# Run with:  mix test test/record_parser_test.exs
#
# The suite progresses from the simplest record constructs to the fully‑fledged
# sample taken from the Nova source tree.  As the grammar grows you can uncomment
# (or refine) the `@tag :pending` tests to lock in behaviour.

# ─────────────────────────────────────────────────────────────────────────────
#  Helpers
# ─────────────────────────────────────────────────────────────────────────────

defmodule Nova.Compiler.RecordParserTest do
  use ExUnit.Case, async: true

  alias Nova.Compiler.{Tokenizer, Parser, Ast}

  @moduledoc false

  # Parse and raise on error so failures show the reason in the diff
  defp parse!(src) do
    tokens = Tokenizer.tokenize(src)

    case Parser.parse(tokens) do
      {:ok, ast} -> ast
      {:error, reason} -> flunk("parse failed: #{inspect(reason)}")
    end
  end

  # Convenience for pulling the sole declaration out of a dummy module wrapper
  defp one_decl!(src) do
    ("module Dummy where\n" <> src)
    |> parse!()
    |> then(fn %Ast.Module{declarations: [d]} -> d end)
  end

  # ───────────────────────────────────────────────────────────────────────────
  # 1. Record ‑ *type* alias  (no layout tricks)                               
  # ───────────────────────────────────────────────────────────────────────────
  test "parses flat record type alias" do
    src = """
    type Pos = { line :: Int, col :: Int }
    """

    %Ast.TypeAlias{name: "Pos", type: %Ast.RecordType{fields: fields}} = one_decl!(src)

    assert fields == [
             {"line", %Ast.Identifier{name: "Int"}},
             {"col", %Ast.Identifier{name: "Int"}}
           ]
  end

  # ───────────────────────────────────────────────────────────────────────────
  # 2. Record type alias with layout newlines & leading commas                 
  # ───────────────────────────────────────────────────────────────────────────
  test "parses multiline record type alias with hanging commas" do
    src = """
    type Foo =
      { a :: String
      , b :: Number
      }
    """

    %Ast.TypeAlias{type: %Ast.RecordType{fields: fields}} = one_decl!(src)
    assert Enum.map(fields, &elem(&1, 0)) == ["a", "b"]
  end

  # ───────────────────────────────────────────────────────────────────────────
  # 3. Record **pattern** in a case‑clause                                      
  # ───────────────────────────────────────────────────────────────────────────
  test "parses record pattern inside ‘Just { … }’ constructor" do
    src = """
    f x =
      case x of
        Just { head, tail } -> head
        _ -> 0
    """

    %Ast.FunctionDeclaration{body: %Ast.CaseExpression{cases: cases}} = one_decl!(src)
    assert length(cases) == 2

    [%Ast.CaseClause{pattern: pat} | _] = cases

    assert %Ast.FunctionCall{
             function: %Ast.Identifier{name: "Just"},
             arguments: [%Ast.RecordPattern{fields: rp_fields}]
           } = pat

    assert Enum.map(rp_fields, &elem(&1, 0)) == ["head", "tail"]
  end

  # ───────────────────────────────────────────────────────────────────────────
  # 4. **Record literal** – pending until the literal grammar lands            
  # ───────────────────────────────────────────────────────────────────────────
  @tag :pending
  test "parses simple record literal" do
    src = """
    x = { foo: 1, bar: 2 }
    """

    # should not raise once parse_record_literal/1 is available
    one_decl!(src)
  end

  # ───────────────────────────────────────────────────────────────────────────
  # 5. Full snippet from Nova.Compiler.Tokenizer                               
  # ───────────────────────────────────────────────────────────────────────────
  test "parses consumeIdentifier full snippet" do
    snippet = """
    module Nova.Compiler.Tokenizer where

    consumeIdentifier source acc pos =
      case uncons $ toCharArray source of
        Nothing -> { value: acc, newSource: "", newPos: pos }
        Just { head, tail } ->
          if isIdentChar head then
            consumeIdentifier (fromCharArray tail) (acc <> singleton head)
              { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
          else
            { value: acc
            , newSource: source
            , newPos: { line: pos.line, column: pos.column, pos: pos.pos }
            }
    """

    %Ast.Module{name: %{name: "Nova.Compiler.Tokenizer"}, declarations: [func]} = parse!(snippet)
    assert %Ast.FunctionDeclaration{name: "consumeIdentifier"} = func

    # basic smoke tests:
    assert length(func.parameters) == 3
    assert %Ast.CaseExpression{} = func.body
  end

  test "parses case" do
    snippet = """
     module Nova.Compiler.Tokenizer where

     consumeIdentifier source acc pos =
       case uncons $ toCharArray source of
         Nothing -> { value: acc, newSource: "", newPos: pos }
         Just { head, tail } ->
          1
    """

    %Ast.Module{declarations: [func]} = parse!(snippet)
  end

  test "parses operator '$' application" do
    src = """
    y = uncons $ toCharArray source
    """

    ast = one_decl!(src)
    assert %Ast.FunctionDeclaration{body: %Ast.FunctionCall{}} = ast
  end
end
