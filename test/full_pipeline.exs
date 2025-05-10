defmodule Nova.IntegrationPipelineTest do
  use ExUnit.Case, async: false

  alias Nova.Compiler.{Tokenizer, Parser, CodeGen}

  @doc """
  Compile one Nova source string all the way to a loaded Elixir module
  and invoke `fun/arity` with `args`, returning the value.
  """
  defp compile_and_eval(source, fun \\ :main, args \\ []) do
    tokens = Tokenizer.tokenize(source)

    case Parser.parse(tokens) do
      {:ok, ast} ->
        # If you already wired the type-checker, uncomment:
        # {:ok, _env} = Nova.Compiler.TypeChecker.check_module(ast)

        elixir_code = CodeGen.compile(ast)
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

  # ─────────────────────────────────────────────────────────────
  #  25 step-by-step integration tests
  # ─────────────────────────────────────────────────────────────

  test "01 - literal integer" do
    src = """
    module NovaTest01 where
    main = 42
    """

    assert compile_and_eval(src) == 42
  end

  test "02 - simple addition" do
    src = """
    module NovaTest02 where
    main = 1 + 2
    """

    assert compile_and_eval(src) == 3
  end

  test "03 - operator precedence" do
    src = """
    module NovaTest03 where
    main = 1 + 2 * 3
    """

    assert compile_and_eval(src) == 7
  end

  test "04 - parentheses override precedence" do
    src = """
    module NovaTest04 where
    main = (1 + 2) * 3
    """

    assert compile_and_eval(src) == 9
  end

  test "05 - boolean guard with if/then/else" do
    src = """
    module NovaTest05 where
    main = if true then 1 else 2
    """

    assert compile_and_eval(src) == 1
  end

  test "06 - equality comparison returns Bool" do
    src = """
    module NovaTest06 where
    main = 3 == 4
    """

    assert compile_and_eval(src) == false
  end

  test "07 - let binding in expression" do
    src = """
    module NovaTest07 where
    main = let x = 5 in x * 2
    """

    assert compile_and_eval(src) == 10
  end

  test "08 - immediate λ-application" do
    src = """
    module NovaTest08 where
    main = (\\x -> x + 1) 4
    """

    assert compile_and_eval(src) == 5
  end

  test "09 - curried λ returns λ" do
    src = """
    module NovaTest09 where
    add = \\x -> \\y -> x + y
    main = add 2 3
    """

    assert compile_and_eval(src) == 5
  end

  test "10 - named function definition & call" do
    src = """
    module NovaTest10 where
    add x y = x + y
    main = add 3 4
    """

    assert compile_and_eval(src) == 7
  end

  test "11 - partial application creates new function" do
    src = """
    module NovaTest11 where
    add x y = x + y
    inc = add 1
    main = inc 5
    """

    assert compile_and_eval(src) == 6
  end

  test "12 - higher-order apply" do
    src = """
    module NovaTest12 where
    apply f x = f x
    main = apply (\\n -> n * 3) 2
    """

    assert compile_and_eval(src) == 6
  end

  test "13 - recursive factorial" do
    src = """
    module NovaTest13 where
    factorial n = if n == 0 then 1 else n * factorial (n - 1)
    main = factorial 5
    """

    assert compile_and_eval(src) == 120
  end

  test "14 - list literal" do
    src = """
    module NovaTest14 where
    main = [1, 2, 3]
    """

    assert compile_and_eval(src) == [1, 2, 3]
  end

  test "15 - list concatenation with ++" do
    src = """
    module NovaTest15 where
    main = [1, 2] ++ [3, 4]
    """

    assert compile_and_eval(src) == [1, 2, 3, 4]
  end

  test "16 - tuple literal" do
    src = """
    module NovaTest16 where
    main = Tuple 1 2
    """

    assert compile_and_eval(src) == {1, "a"}
  end

  test "17 - case expression with constant patterns" do
    src = """
    module NovaTest17 where
    main = case 1 of
      1 -> "one"
      _ -> "other"
    """

    assert compile_and_eval(src) == "one"
  end

  test "18 - nested let bindings" do
    src = """
    module NovaTest18 where
    main = let x = 2 in let y = 3 in x + y
    """

    assert compile_and_eval(src) == 5
  end

  test "19 - boolean && operator" do
    src = """
    module NovaTest19 where
    main = if true && false then 1 else 0
    """

    assert compile_and_eval(src) == 0
  end

  test "20 - numeric comparison > inside if" do
    src = """
    module NovaTest20 where
    main = if 5 > 3 then "yes" else "no"
    """

    assert compile_and_eval(src) == "yes"
  end

  test "21 - λ capturing outer let-bound variable" do
    src = """
    module NovaTest21 where
    main = let x = 10 in (\\y -> x + y) 5
    """

    assert compile_and_eval(src) == 15
  end

  test "22 - string concatenation with ++" do
    src = """
    module NovaTest22 where
    main = "foo" ++ "bar"
    """

    assert compile_and_eval(src) == "foobar"
  end

  test "23 - tuple swap via pattern match in head" do
    src = """
    module NovaTest23 where
    swap {a, b} = {b, a}
    main = swap {1, 2}
    """

    assert compile_and_eval(src) == {2, 1}
  end

  test "24 - exponentiation via recursive pow" do
    src = """
    module NovaTest24 where
    pow x y = if y == 0 then 1 else x * pow x (y - 1)
    main = pow 2 3
    """

    assert compile_and_eval(src) == 8
  end

  test "25 - local λ in let binding" do
    src = """
    module NovaTest25 where
    main = let f = \\n -> n - 1 in f 4
    """

    assert compile_and_eval(src) == 3
  end
end
