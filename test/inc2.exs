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

  @section_delimiter "-------------------#section-------------------"

  defp split_sections(src) do
    src
    |> String.split(@section_delimiter, trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp process_chunk(chunk, ns, reg) do
    with tokens <- Tokenizer.tokenize(chunk),
         {:ok, decls, _} <- Parser.parse_declarations(tokens) do
      case TypeChecker.check_block(decls, ns, reg) do
        {:ok, dec, _} ->
          dec

        {:error, reason} ->
          flunk("Type error in declaration #{inspect(decls)}: #{inspect(reason)}")
      end
    else
      {:error, reason} ->
        flunk("parse_error: #{inspect(reason)}")
    end
    |> case do
      {:error, _} = err ->
        flunk("other_error: #{inspect(err)}")

      x ->
        x
    end
  end

  @source File.read!("lib/ast.nv")
  test "incrementally adds each declaration to the environment" do
    root = Nova.InterfaceRegistry.new_root!()
    layer = Nova.InterfaceRegistry.begin_batch(root)

    [head | rest] =
      @source
      |> split_sections()

    tokens = Tokenizer.tokenize(head)
    {:ok, module, []} = Parser.parse_module(tokens)
    IO.inspect module.name.name

    # lets get the module from there ^

    env = Nova.Compiler.Types.Env.empty()
    # gotta set the current module to module

    rest
    |> Enum.reduce(env, fn s, e ->
      IO.puts("# adding chunk")
      IO.puts(s)
      process_chunk(s, :"#{module.name.name}", layer)
    end)
  end
end
