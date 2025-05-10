defmodule Nova.TypeChecker.IncrementalTest do
  use ExUnit.Case
  alias Nova.Compiler.{Tokenizer, Parser, TypeChecker}
  alias Nova.Compiler.Env

"""
module Nova.Compiler.Tokenizer where

  consumeIdentifier source acc pos =
    case uncons $ toCharArray source of
      Nothing -> { value: acc, newSource: "", newPos: pos }
      Just { head, tail } -> 
"""

  @source """
case isIdentChar head of
  true -> 
     2
  false -> 
     1         
  """

  test "incrementally adds each declaration to the environment" do
    tokens = IO.inspect(Tokenizer.tokenize(@source))
    {:ok, decls, rest} = IO.inspect(Parser.parse_declarations(tokens))
    IO.inspect decls
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
