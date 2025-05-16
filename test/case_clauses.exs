defmodule TH2Test do
  use ExUnit.Case

  case_clauses = [
    "Just { head: tok, tail: rest } | tok.t_type == NewLine -> skipNewlines rest",
    "_ -> ts"
  ]

  Enum.map(case_clauses, fn s ->
    test "case clauses start: #{s}" do
      tokens = Nova.Compiler.Tokenizer.tokenize(unquote(s))
      res = Nova.Compiler.Parser.clause_start?(tokens)

      if res == false do
        IO.inspect(tokens)
      end

      true = res
    end
  end)
end
