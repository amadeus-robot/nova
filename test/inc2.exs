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

  @type error ::
          {:parse_error, term()}
          | {:type_error, Nova.Compiler.Ast.Declaration.t(), term()}

  @doc """
  Run incremental type-checking on *path*.

  * `{:ok, env}`  – all chunks succeeded, returning the final `Env`.
  * `{:error, error}` – first parse/type error encountered.
  """
  @spec run(Path.t()) :: {:ok, Env.t()} | {:error, error()}
  def run(path) when is_binary(path) do
    path
    |> File.read!()
    |> split_sections()
    |> Enum.reduce_while(Env.empty(), &process_chunk/2)
  end

  @doc """
  Same as `run/1` but raises on the first error.
  """
  @spec run!(Path.t()) :: Env.t() | no_return()
  def run!(path) do
    case run(path) do
      {:ok, env} ->
        env

      {:error, {:parse_error, reason}} ->
        raise "parse error: #{inspect(reason)}"

      {:error, {:type_error, decl, reason}} ->
        raise "type error in #{inspect(decl)}: #{inspect(reason)}"
    end
  end

  # ──────────────────────────────────────────────────────────────
  # Internals
  # ──────────────────────────────────────────────────────────────

  defp split_sections(src) do
    src
    |> String.split(@section_delimiter, trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp process_chunk(chunk, env) do
    with tokens <- Tokenizer.tokenize(chunk),
         {:ok, decls, _} <- Parser.parse_declarations(tokens) do
      Enum.reduce(decls, env, fn decl, env_acc ->
        case TypeChecker.check_declaration(decl, env_acc) do
          {:ok, dec, new_env} ->
            new_env

          {:error, reason} ->
            flunk("Type error in declaration #{inspect(decl)}: #{inspect(reason)}")
        end
      end)
    else
      {:error, reason} ->
        flunk("parse_error: #{inspect(reason)}")
    end
    |> case do
      {:error, _} = err ->
        flunk("other_error: #{inspect(err)}")

      new_env ->
        new_env
    end
  end

  @source File.read!("lib/parser.nv")
  test "incrementally adds each declaration to the environment" do
    [head | rest] =
      @source
      |> split_sections()

    tokens = Tokenizer.tokenize(head)
    {:ok, module, []} = Parser.parse_module(tokens)

    env = Nova.Compiler.Types.Env.empty()
    # gotta set the current module to module

    rest
    |> Enum.reduce(env, fn s, e ->
      IO.puts("# adding chunk")
      IO.puts(s)
      process_chunk(s, e)
    end)
  end
end
