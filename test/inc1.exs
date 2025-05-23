defmodule Nova.TypeChecker.IncrementalTest do
  use ExUnit.Case
  alias Nova.Compiler.{Tokenizer, Parser, TypeChecker}
  alias Nova.Compiler.Env

  @source File.read!("lib/tokenizer.nv")
  test "incrementally adds each declaration to the environment" do
    IO.puts(@source)
    tokens = IO.inspect(Tokenizer.tokenize(@source))
    {:ok, decls, rest} = IO.inspect(Parser.parse_module(tokens))
    assert rest == []
    IO.inspect(decls)
    IO.inspect(rest)
    # {:ok, %Nova.Compiler.Ast.Module{body: decls}, []} = Parser.parse_module(tokens)

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
