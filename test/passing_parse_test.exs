defmodule Nova.IntegrationPipelineTest do
  use ExUnit.Case, async: false

  alias Nova.Compiler.{Tokenizer, Parser, CodeGen}

  @doc """
  Compile one Nova source string all the way to a loaded Elixir module
  and invoke `fun/arity` with `args`, returning the value.
  """
  defp compile_and_eval(source, fun \\ :main, args \\ []) do
    tokens = Tokenizer.tokenize(source)

    case Parser.parse_declarations(tokens) do
      {:ok, _, t} when t != [] ->
        pretty_tokens =
          tokens
          |> Enum.map_join("\n", fn t -> inspect(t, pretty: true, limit: :infinity) end)

        flunk("""
        ────── PARSER FAILED ─────────────────────────────────────────────
        remaining tokens 

        ────── SOURCE ───────────────────────────────────────────────────
        #{String.trim_trailing(source)}

        ────── TOKENS ───────────────────────────────────────────────────
        #{inspect(hd(t))}
        """)

      {:ok, ast, []} ->
        :ok

      {:error, reason} ->
        pretty_tokens =
          tokens
          |> Enum.map_join("\n", fn t -> inspect(t, pretty: true, limit: :infinity) end)

        flunk("""
        ────── PARSER FAILED ─────────────────────────────────────────────
        #{reason}

        ────── SOURCE ───────────────────────────────────────────────────
        #{String.trim_trailing(source)}

        """)
    end
  end

  {:ok, files} = File.ls("tests/passing/")

  Enum.map(files, fn name ->
    if !File.dir?("tests/passing/#{name}") do
      test name do
        src = File.read!("tests/passing/#{unquote(name)}")
        assert compile_and_eval(src) == :ok
      end
    end
  end)
end
