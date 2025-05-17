defmodule TH do
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

  defp process_chunk(chunk, ns, reg, env) do
    # ,
    # {:ok, _} <- Nova.Compiler.NameResolver.resolve_block(decls, ns, reg, env) 
    with tokens <- Nova.Compiler.Tokenizer.tokenize(chunk),
         {:ok, decls, _} <- Nova.Compiler.Parser.parse_declarations(tokens) do
      case IO.inspect(Nova.Compiler.TypeChecker.check_block(decls, ns, reg)) do
        {:ok, dec, something} ->
          IO.inspect(something, limit: 88_888_888_888)
          dec

        {:error, reason} ->
          raise("Type error in declaration #{inspect(decls)}: #{inspect(reason)}")
      end
    else
      {:error, reason} ->
        raise("parse_error: #{inspect(reason)}")
    end
    |> case do
      {:error, _} = err ->
        raise("other_error: #{inspect(err)}")

      x ->
        x
    end
  end

  def test1 do
    root = Nova.Compiler.InterfaceRegistry.new_root!()
    layer = Nova.Compiler.InterfaceRegistry.begin_batch(root)

    source = File.read!("lib/ast.nv")

    tokens = Nova.Compiler.Tokenizer.tokenize(source)

    {:ok,
     %Nova.Compiler.Ast.Module{
       name: %Nova.Compiler.Ast.Identifier{name: namespace},
       declarations: nil
     }, tokens} = Nova.Compiler.Parser.parse_module(tokens)

    IO.inspect(namespace)
    env = %{Nova.Compiler.Types.Env.empty() | registry_layer: layer}

    # rest
    # |> Enum.reduce(env, fn s, env ->
    #  IO.puts("# adding chunk")
    #  IO.puts(s)
    #  process_chunk(s, :"#{namespace}", layer, env)
    # end)
  end
end
