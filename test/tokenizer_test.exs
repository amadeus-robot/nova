defmodule Nova.Compiler.TokenizerTest do
  use ExUnit.Case

  alias Nova.Compiler.Tokenizer

  describe "tokenize/1" do
    test "tokenizes namespace declaration" do
      input = "namespace rolf.lol"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "namespace", {1, 1}},
               {:identifier, "rolf.lol", {1, 11}}
             ]
    end

    test "tokenizes type declaration" do
      input = "type Token = \n  | IntLiteral(Int)"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {1, 1}},
               {:identifier, "Token", {1, 6}},
               {:operator, "=", {1, 12}},
               {:pipe, "|", {2, 3}},
               {:identifier, "IntLiteral", {2, 5}},
               {:left_paren, "(", {2, 15}},
               {:identifier, "Int", {2, 16}},
               {:right_paren, ")", {2, 19}}
             ]
    end

    test "tokenizes integer literals" do
      input = "42 123 0"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:int_literal, 42, {1, 1}},
               {:int_literal, 123, {1, 4}},
               {:int_literal, 0, {1, 8}}
             ]
    end

    test "tokenizes float literals" do
      input = "3.14 0.5 42.0"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:float_literal, "3.14", {1, 1}},
               {:float_literal, "0.5", {1, 6}},
               {:float_literal, "42.0", {1, 10}}
             ]
    end

    test "tokenizes string literals" do
      input = "\"hello\" \"world\" \"with \\\"quotes\\\"\""

      result = Tokenizer.tokenize(input)

      assert result == [
               {:string_literal, "hello", {1, 1}},
               {:string_literal, "world", {1, 9}},
               {:string_literal, "with \"quotes\"", {1, 17}}
             ]
    end

    test "tokenizes boolean literals" do
      input = "true false"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:bool_literal, "true", {1, 1}},
               {:bool_literal, "false", {1, 6}}
             ]
    end

    test "tokenizes identifiers" do
      input = "x foo Bar_123"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:identifier, "x", {1, 1}},
               {:identifier, "foo", {1, 3}},
               {:identifier, "Bar_123", {1, 7}}
             ]
    end

    test "tokenizes type variables" do
      input = "'a 'b 'TypeVar"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:type_var, "a", {1, 1}},
               {:type_var, "b", {1, 4}},
               {:type_var, "TypeVar", {1, 7}}
             ]
    end

    test "tokenizes keywords" do
      input = "type namespace with"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {1, 1}},
               {:keyword, "namespace", {1, 6}},
               {:keyword, "with", {1, 16}}
             ]
    end

    test "tokenizes operators" do
      input = "-> | : +"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:arrow, "->", {1, 1}},
               {:pipe, "|", {1, 4}},
               {:colon, ":", {1, 6}},
               {:operator, "+", {1, 8}}
             ]
    end

    test "tokenizes delimiters" do
      input = "() {} ,"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:left_paren, "(", {1, 1}},
               {:right_paren, ")", {1, 2}},
               {:left_brace, "{", {1, 4}},
               {:right_brace, "}", {1, 5}},
               {:comma, ",", {1, 7}}
             ]
    end

    test "tokenizes complex type definition" do
      input = """
      type ParseResult('a) = Success('a, List(Token)) | Error(String)
      """

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {1, 1}},
               {:identifier, "ParseResult", {1, 6}},
               {:left_paren, "(", {1, 17}},
               {:type_var, "a", {1, 18}},
               {:right_paren, ")", {1, 20}},
               {:operator, "=", {1, 22}},
               {:identifier, "Success", {1, 24}},
               {:left_paren, "(", {1, 31}},
               {:type_var, "a", {1, 32}},
               {:comma, ",", {1, 34}},
               {:identifier, "List", {1, 36}},
               {:left_paren, "(", {1, 40}},
               {:identifier, "Token", {1, 41}},
               {:right_paren, ")", {1, 46}},
               {:right_paren, ")", {1, 47}},
               {:pipe, "|", {1, 49}},
               {:identifier, "Error", {1, 51}},
               {:left_paren, "(", {1, 56}},
               {:identifier, "String", {1, 57}},
               {:right_paren, ")", {1, 63}}
             ]
    end

    test "handles comments" do
      input = """
      # This is a comment
      type Token # Another comment
      """

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {2, 1}},
               {:identifier, "Token", {2, 6}}
             ]
    end

    test "handles multiline input" do
      input = """
      namespace gr.compiler

      type Token = 
        | IntLiteral(Int)
      """

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "namespace", {1, 1}},
               {:identifier, "gr.compiler", {1, 11}},
               {:keyword, "type", {3, 1}},
               {:identifier, "Token", {3, 6}},
               {:operator, "=", {3, 12}},
               {:pipe, "|", {4, 3}},
               {:identifier, "IntLiteral", {4, 5}},
               {:left_paren, "(", {4, 15}},
               {:identifier, "Int", {4, 16}},
               {:right_paren, ")", {4, 19}}
             ]
    end

    test "handles record type syntax" do
      input = """
      type TypeDefinition = {
        name: String,
        params: List(String),
        body: TypeExpr
      }
      """

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {1, 1}},
               {:identifier, "TypeDefinition", {1, 6}},
               {:operator, "=", {1, 22}},
               {:left_brace, "{", {1, 24}},
               {:identifier, "name", {2, 3}},
               {:colon, ":", {2, 7}},
               {:identifier, "String", {2, 9}},
               {:comma, ",", {2, 15}},
               {:identifier, "params", {3, 3}},
               {:colon, ":", {3, 9}},
               {:identifier, "List", {3, 11}},
               {:left_paren, "(", {3, 15}},
               {:identifier, "String", {3, 16}},
               {:right_paren, ")", {3, 22}},
               {:comma, ",", {3, 23}},
               {:identifier, "body", {4, 3}},
               {:colon, ":", {4, 7}},
               {:identifier, "TypeExpr", {4, 9}},
               {:right_brace, "}", {5, 1}}
             ]
    end

    test "handles constrained type syntax" do
      input = "'a with Num"

      result = Tokenizer.tokenize(input)

      assert result == [
               {:type_var, "a", {1, 1}},
               {:keyword, "with", {1, 4}},
               {:identifier, "Num", {1, 9}}
             ]
    end

    test "maintains line and column information" do
      input = """
      type
        Token =
          | IntLiteral
      """

      result = Tokenizer.tokenize(input)

      assert result == [
               {:keyword, "type", {1, 1}},
               {:identifier, "Token", {2, 3}},
               {:operator, "=", {2, 9}},
               {:pipe, "|", {3, 5}},
               {:identifier, "IntLiteral", {3, 7}}
             ]
    end
  end
end
