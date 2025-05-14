defmodule Nova.TypeChecker.IncrementalTest do
  use ExUnit.Case
  alias Nova.Compiler.{Tokenizer, Parser, TypeChecker}
  alias Nova.Compiler.Env

  @moduledoc """
  Incrementally parses and type-checks a Nova source file whose logical
  chunks are delimited by the literal line

      -------------------#section-------------------

  The typing environment is threaded from one chunk to the next; the
  first parse or type error aborts the run.

      iex> Nova.Compiler.Incremental.run!("lib/parser.nv")
      :ok
  """

  test "incrementally adds each declaration to the environment" do
    root = Nova.InterfaceRegistry.new_root!()
    layer = Nova.InterfaceRegistry.begin_batch(root)

    source = File.read!("lib/ast.nv")

    [head | rest] =
      source |> split_sections()

    tokens = Nova.Compiler.Tokenizer.tokenize(head)

    {:ok,
     %Nova.Compiler.Ast.Module{
       name: %Nova.Compiler.Ast.Identifier{name: namespace},
       declarations: nil
     }, tokens} = Nova.Compiler.Parser.parse_module(tokens)

    IO.inspect(namespace)

    rest
    |> Enum.reduce(env, fn s, e ->
      IO.puts("# adding chunk")
      IO.puts(s)
      process_chunk(s, :"#{namespace}", layer)
    end)
  end
end
