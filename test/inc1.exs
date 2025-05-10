defmodule Nova.TypeChecker.IncrementalTest do
  use ExUnit.Case
  alias Nova.Compiler.{Tokenizer, Parser, TypeChecker}
  alias Nova.Compiler.Env

  @source """
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

  test "incrementally adds each declaration to the environment" do
    tokens = IO.inspect(Tokenizer.tokenize(@source))
    {:ok, %{body: decls}} = IO.inspect(Parser.parse(tokens))
    # {:ok, %Nova.Compiler.Ast.Module{body: decls}} = Parser.parse(tokens)

    # Start with empty type environment
    Enum.reduce(decls, Env.empty(), fn decl, env ->
      case TypeChecker.check_declaration(decl, env) do
        {:ok, _type, _subst, updated_env} ->
          updated_env

        {:error, reason} ->
          flunk("Type error in declaration #{inspect(decl)}: #{inspect(reason)}")
      end
    end)
  end
end
