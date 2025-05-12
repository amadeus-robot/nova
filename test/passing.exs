defmodule Nova.IntegrationPipelineTest do
  use ExUnit.Case, async: false

  alias Nova.Compiler.{Tokenizer, Parser, CodeGen}

  @doc """
  Compile one Nova source string all the way to a loaded Elixir module
  and invoke `fun/arity` with `args`, returning the value.
  """
  defp compile_and_eval(source, fun \\ :main, args \\ []) do
    tokens = Tokenizer.tokenize(source)

    case Parser.parse_module(tokens) do
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
        #{pretty_tokens}
        """)

      {:ok, ast, []} ->
        # If you already wired the type-checker, uncomment:
        # {:ok, _env} = Nova.Compiler.TypeChecker.check_module(ast)

        elixir_code = CodeGen.compile(ast)

        IO.puts(
          " ────── compiled ─────────────────────────────────────────────\n" <>
            elixir_code <>
            "\n ───────────────────────────────────────────────────\n"
        )

        [{mod, _bin}] = Code.compile_string(elixir_code)
        apply(mod, fun, args)

      {:error, reason} ->
        pretty_tokens =
          tokens
          |> Enum.map_join("\n", fn t -> inspect(t, pretty: true, limit: :infinity) end)

        flunk("""
        ────── PARSER FAILED ─────────────────────────────────────────────
        #{reason}

        ────── SOURCE ───────────────────────────────────────────────────
        #{String.trim_trailing(source)}

        ────── TOKENS ───────────────────────────────────────────────────
        #{pretty_tokens}
        """)
    end
  end

  {:ok, files} = File.ls("tests/passing/")

  Enum.map(files, fn name ->
    test name do
      src = File.read!("tests/passing/#{unquote(name)}")

      assert compile_and_eval(src) == "Done"
    end
  end)
end
