defmodule Nova.Compiler.RecordParserTest do
  use ExUnit.Case, async: true

  alias Nova.Compiler.{Tokenizer, Parser, Ast}

  @doc """
  Convenience that returns the raw source → {:ok, ast} or raises with the
  original error so the failing test prints a nice message.
  """
  defp parse!(src) do
    tokens = Tokenizer.tokenize(src)

    case Parser.parse_declarations(tokens) do
      {:ok, [ast], []} -> ast
      {:error, _} ->
        case Parser.parse_expression(tokens) do
          {:ok, ast, []} -> ast
          {:ok, _ast, rest} -> flunk("unparsed tokens: #{inspect(rest)}")
          {:error, reason} -> flunk("parser error: #{inspect(reason)}")
        end
      {:ok, _ast_list, rest} -> flunk("unparsed tokens: #{inspect(rest)}")
    end
  end

  # ────────────────────────────────────────────────────────────────
  # 1. Single‑field record type alias (one‑liner)
  # ────────────────────────────────────────────────────────────────
  test "single‑field record type alias" do
    src = """
    type Person = { name :: String }          
    """

    ast = parse!(src)
    assert %Ast.TypeAlias{name: "Person"} = ast
  end

  # ────────────────────────────────────────────────────────────────
  # 2. Multi‑line, multi‑field record type alias (layout breaks)
  # ────────────────────────────────────────────────────────────────
  test "multi‑line record type alias with newlines" do
    src = """
    type Position =
      { line   :: Int
      , column :: Int
      , pos    :: Int
      }
    """

    ast = parse!(src)

    assert %Ast.TypeAlias{
             name: "Position",
             type: %Ast.RecordType{fields: fields}
           } = ast

    assert [{"line", _}, {"column", _}, {"pos", _}] = fields
  end

  # ────────────────────────────────────────────────────────────────
  # 3. Record pattern inside a case‑expression (field punning)
  # ────────────────────────────────────────────────────────────────
  test "record pattern punning in case clause" do
    src = """
    case token of
      Just { head, tail } -> head
    """

    ast = parse!(src)
    assert %Ast.CaseExpression{cases: [%Ast.CaseClause{pattern: %Ast.FunctionCall{}}]} = ast
  end

  # ────────────────────────────────────────────────────────────────
  # 4. Record pattern with explicit field = pattern pair
  # ────────────────────────────────────────────────────────────────
  test "record pattern with explicit assignment" do
    src = """
    case token of
      Just { head = h, tail = ts } -> h
    """

    ast = parse!(src)

    assert %Ast.CaseExpression{
             cases: [%Ast.CaseClause{pattern: %Ast.FunctionCall{arguments: [rec_pat | _]}} | _]
           } = ast

    assert %Ast.RecordPattern{fields: [{"head", _}, {"tail", _}]} = rec_pat
  end
end
