defmodule TH2 do
  def flunk(a) do
    raise a
  end

  def go do
    s = """
    skipNewlines :: Tokens -> Tokens
    skipNewlines ts = case uncons ts of
        Just { head: tok, tail: rest } | tok.t_type == NewLine -> skipNewlines rest
        _ -> ts
    """

    tokens = Nova.Compiler.Tokenizer.tokenize(s)

    case Nova.Compiler.Parser.parse_declarations(tokens) do
      {:ok, _, t} when t != [] ->
        pretty_tokens =
          tokens
          |> Enum.map_join("\n", fn t -> inspect(t, pretty: true, limit: :infinity) end)

        flunk("""
        ────── PARSER FAILED ─────────────────────────────────────────────
        remaining tokens 

        ────── TOKENS ───────────────────────────────────────────────────
        #{inspect(hd(t))}
        """)

      {:ok, ast, []} ->
        # If you already wired the type-checker, uncomment:
        # {:ok, _env} = Nova.Compiler.TypeChecker.check_module(ast)

        elixir_code = Nova.Compiler.CodeGen.compile(ast)

        IO.puts(
          " ────── compiled ─────────────────────────────────────────────\n" <>
            elixir_code <>
            "\n ───────────────────────────────────────────────────\n"
        )

        [{mod, _bin}] = Code.compile_string(elixir_code)
        # apply(mod, fun, args)
    end
  end
end
