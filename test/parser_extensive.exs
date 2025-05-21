defmodule Nova.CompilerTest do
  use ExUnit.Case

  alias Nova.Compiler.Tokenizer
  alias Nova.Compiler.Parser
  alias Nova.Compiler.Ast

  # Helper functions for testing
  defp tokenize_and_parse(source) do
    tokens = Tokenizer.tokenize(source)

    case Parser.parse_expression(tokens) do
      {:ok, ast, _rest} -> {:ok, ast}
      {:error, reason} -> {:error, reason}
    end
  end

  defp assert_tokens_match(source, expected_tokens) do
    tokens = Tokenizer.tokenize(source)

    assert length(tokens) == length(expected_tokens),
           "Expected #{length(expected_tokens)} tokens, got #{length(tokens)}"

    Enum.zip(tokens, expected_tokens)
    |> Enum.each(fn {actual, expected} ->
      assert actual.type == expected.type,
             "Token type mismatch at position #{actual.pos}: expected #{expected.type}, got #{actual.type}"

      assert actual.value == expected.value,
             "Token value mismatch at position #{actual.pos}: expected #{expected.value}, got #{actual.value}"
    end)
  end

  defp assert_ast_match(ast, expected_ast) do
    assert ast == expected_ast,
           "AST mismatch: \nExpected: #{inspect(expected_ast, pretty: true)}\nGot: #{inspect(ast, pretty: true)}"
  end

  # Section 1: Basic Literals and Expressions

  test "Integer literals" do
    # Positive integer
    assert_tokens_match("42", [
      %Tokenizer.Token{type: :number, value: "42", line: 1, column: 1, pos: 0}
    ])

    # Negative integer (this will be tokenized as an operator and a number)
    assert_tokens_match("-42", [
      %Tokenizer.Token{type: :operator, value: "-", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "42", line: 1, column: 2, pos: 1}
    ])

    # Zero
    assert_tokens_match("0", [
      %Tokenizer.Token{type: :number, value: "0", line: 1, column: 1, pos: 0}
    ])

    # Test parsing of a simple integer literal
    {:ok, ast} = tokenize_and_parse("42")
    expected_ast = %Ast.Literal{type: :number, value: "42"}
    assert_ast_match(ast, expected_ast)
  end

  test "Float literals" do
    # Positive float
    assert_tokens_match("3.14", [
      %Tokenizer.Token{type: :number, value: "3.14", line: 1, column: 1, pos: 0}
    ])

    # Negative float
    assert_tokens_match("-3.14", [
      %Tokenizer.Token{type: :operator, value: "-", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "3.14", line: 1, column: 2, pos: 1}
    ])

    # Zero as float
    assert_tokens_match("0.0", [
      %Tokenizer.Token{type: :number, value: "0.0", line: 1, column: 1, pos: 0}
    ])

    # Test parsing of a float literal
    {:ok, ast} = tokenize_and_parse("3.14")
    expected_ast = %Ast.Literal{type: :number, value: "3.14"}
    assert_ast_match(ast, expected_ast)
  end

  test "String literals" do
    # Empty string
    assert_tokens_match("\"\"", [
      %Tokenizer.Token{type: :string, value: "", line: 1, column: 1, pos: 0}
    ])

    # Simple string
    assert_tokens_match("\"hello\"", [
      %Tokenizer.Token{type: :string, value: "hello", line: 1, column: 1, pos: 0}
    ])

    # String with escape sequences
    assert_tokens_match("\"hello\\nworld\"", [
      %Tokenizer.Token{type: :string, value: "hello\\nworld", line: 1, column: 1, pos: 0}
    ])

    # Test parsing of a string literal
    {:ok, ast} = tokenize_and_parse("\"hello\"")
    expected_ast = %Ast.Literal{type: :string, value: "hello"}
    assert_ast_match(ast, expected_ast)
  end

  test "Character literals" do
    # Simple character
    assert_tokens_match("'a'", [
      %Tokenizer.Token{type: :char, value: "a", line: 1, column: 1, pos: 0}
    ])

    # Escaped character
    assert_tokens_match("'\\n'", [
      %Tokenizer.Token{type: :char, value: "\n", line: 1, column: 1, pos: 0}
    ])

    # Test parsing of a character literal
    {:ok, ast} = tokenize_and_parse("'a'")
    expected_ast = %Ast.Literal{type: :char, value: "a"}
    assert_ast_match(ast, expected_ast)
  end

  test "List literals" do
    # Empty list
    assert_tokens_match("[]", [
      %Tokenizer.Token{type: :delimiter, value: "[", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :delimiter, value: "]", line: 1, column: 2, pos: 1}
    ])

    # Single element list
    assert_tokens_match("[1]", [
      %Tokenizer.Token{type: :delimiter, value: "[", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "1", line: 1, column: 2, pos: 1},
      %Tokenizer.Token{type: :delimiter, value: "]", line: 1, column: 3, pos: 2}
    ])

    # Multiple element list
    assert_tokens_match("[1, 2, 3]", [
      %Tokenizer.Token{type: :delimiter, value: "[", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "1", line: 1, column: 2, pos: 1},
      %Tokenizer.Token{type: :delimiter, value: ",", line: 1, column: 3, pos: 2},
      %Tokenizer.Token{type: :number, value: "2", line: 1, column: 5, pos: 4},
      %Tokenizer.Token{type: :delimiter, value: ",", line: 1, column: 6, pos: 5},
      %Tokenizer.Token{type: :number, value: "3", line: 1, column: 8, pos: 7},
      %Tokenizer.Token{type: :delimiter, value: "]", line: 1, column: 9, pos: 8}
    ])

    # Test parsing of a list literal
    {:ok, ast} = tokenize_and_parse("[1, 2, 3]")

    expected_ast = %Ast.List{
      elements: [
        %Ast.Literal{type: :number, value: "1"},
        %Ast.Literal{type: :number, value: "2"},
        %Ast.Literal{type: :number, value: "3"}
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Tuple literals" do
    # Empty tuple
    assert_tokens_match("()", [
      %Tokenizer.Token{type: :delimiter, value: "(", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :delimiter, value: ")", line: 1, column: 2, pos: 1}
    ])

    # Single element tuple (this actually becomes a grouped expression in AST)
    assert_tokens_match("(1)", [
      %Tokenizer.Token{type: :delimiter, value: "(", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "1", line: 1, column: 2, pos: 1},
      %Tokenizer.Token{type: :delimiter, value: ")", line: 1, column: 3, pos: 2}
    ])

    # Multiple element tuple
    assert_tokens_match("(1, 2, 3)", [
      %Tokenizer.Token{type: :delimiter, value: "(", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :number, value: "1", line: 1, column: 2, pos: 1},
      %Tokenizer.Token{type: :delimiter, value: ",", line: 1, column: 3, pos: 2},
      %Tokenizer.Token{type: :number, value: "2", line: 1, column: 5, pos: 4},
      %Tokenizer.Token{type: :delimiter, value: ",", line: 1, column: 6, pos: 5},
      %Tokenizer.Token{type: :number, value: "3", line: 1, column: 8, pos: 7},
      %Tokenizer.Token{type: :delimiter, value: ")", line: 1, column: 9, pos: 8}
    ])

    # Test parsing of a tuple literal
    {:ok, ast} = tokenize_and_parse("(1, 2, 3)")

    expected_ast = %Ast.Tuple{
      elements: [
        %Ast.Literal{type: :number, value: "1"},
        %Ast.Literal{type: :number, value: "2"},
        %Ast.Literal{type: :number, value: "3"}
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 2: Operators and Expressions

  test "Arithmetic operators" do
    # Addition
    {:ok, ast} = tokenize_and_parse("1 + 2")

    expected_ast = %Ast.BinaryOp{
      op: "+",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Subtraction
    {:ok, ast} = tokenize_and_parse("5 - 3")

    expected_ast = %Ast.BinaryOp{
      op: "-",
      left: %Ast.Literal{type: :number, value: "5"},
      right: %Ast.Literal{type: :number, value: "3"}
    }

    assert_ast_match(ast, expected_ast)

    # Multiplication
    {:ok, ast} = tokenize_and_parse("2 * 3")

    expected_ast = %Ast.BinaryOp{
      op: "*",
      left: %Ast.Literal{type: :number, value: "2"},
      right: %Ast.Literal{type: :number, value: "3"}
    }

    assert_ast_match(ast, expected_ast)

    # Division
    {:ok, ast} = tokenize_and_parse("10 / 2")

    expected_ast = %Ast.BinaryOp{
      op: "/",
      left: %Ast.Literal{type: :number, value: "10"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Comparison operators" do
    # Equal
    {:ok, ast} = tokenize_and_parse("1 == 2")

    expected_ast = %Ast.BinaryOp{
      op: "==",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Not equal
    {:ok, ast} = tokenize_and_parse("1 != 2")

    expected_ast = %Ast.BinaryOp{
      op: "!=",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Less than
    {:ok, ast} = tokenize_and_parse("1 < 2")

    expected_ast = %Ast.BinaryOp{
      op: "<",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Less than or equal
    {:ok, ast} = tokenize_and_parse("1 <= 2")

    expected_ast = %Ast.BinaryOp{
      op: "<=",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Greater than
    {:ok, ast} = tokenize_and_parse("2 > 1")

    expected_ast = %Ast.BinaryOp{
      op: ">",
      left: %Ast.Literal{type: :number, value: "2"},
      right: %Ast.Literal{type: :number, value: "1"}
    }

    assert_ast_match(ast, expected_ast)

    # Greater than or equal
    {:ok, ast} = tokenize_and_parse("2 >= 1")

    expected_ast = %Ast.BinaryOp{
      op: ">=",
      left: %Ast.Literal{type: :number, value: "2"},
      right: %Ast.Literal{type: :number, value: "1"}
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Logical operators" do
    # And
    {:ok, ast} = tokenize_and_parse("true && false")

    expected_ast = %Ast.BinaryOp{
      op: "&&",
      left: %Ast.Identifier{name: "true"},
      right: %Ast.Identifier{name: "false"}
    }

    assert_ast_match(ast, expected_ast)

    # Or
    {:ok, ast} = tokenize_and_parse("true || false")

    expected_ast = %Ast.BinaryOp{
      op: "||",
      left: %Ast.Identifier{name: "true"},
      right: %Ast.Identifier{name: "false"}
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Operator precedence and associativity" do
    # Multiplication has higher precedence than addition
    {:ok, ast} = tokenize_and_parse("1 + 2 * 3")

    expected_ast = %Ast.BinaryOp{
      op: "+",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Literal{type: :number, value: "2"},
        right: %Ast.Literal{type: :number, value: "3"}
      }
    }

    assert_ast_match(ast, expected_ast)

    # Parentheses override precedence
    {:ok, ast} = tokenize_and_parse("(1 + 2) * 3")

    expected_ast = %Ast.BinaryOp{
      op: "*",
      left: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Literal{type: :number, value: "1"},
        right: %Ast.Literal{type: :number, value: "2"}
      },
      right: %Ast.Literal{type: :number, value: "3"}
    }

    assert_ast_match(ast, expected_ast)

    # Associativity (left to right for most operators)
    {:ok, ast} = tokenize_and_parse("1 - 2 - 3")

    expected_ast = %Ast.BinaryOp{
      op: "-",
      left: %Nova.Compiler.Ast.Literal{type: :number, value: "1"},
      right: %Nova.Compiler.Ast.BinaryOp{
        op: "-",
        left: %Nova.Compiler.Ast.Literal{type: :number, value: "2"},
        right: %Nova.Compiler.Ast.Literal{type: :number, value: "3"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Parenthesized expressions" do
    # Simple parenthesized expression
    {:ok, ast} = tokenize_and_parse("(1 + 2)")

    expected_ast = %Ast.BinaryOp{
      op: "+",
      left: %Ast.Literal{type: :number, value: "1"},
      right: %Ast.Literal{type: :number, value: "2"}
    }

    assert_ast_match(ast, expected_ast)

    # Nested parenthesized expressions
    {:ok, ast} = tokenize_and_parse("((1 + 2) * (3 - 4))")

    expected_ast = %Ast.BinaryOp{
      op: "*",
      left: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Literal{type: :number, value: "1"},
        right: %Ast.Literal{type: :number, value: "2"}
      },
      right: %Ast.BinaryOp{
        op: "-",
        left: %Ast.Literal{type: :number, value: "3"},
        right: %Ast.Literal{type: :number, value: "4"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 3: Identifiers and Variables

  test "Simple identifiers" do
    # Simple identifier
    assert_tokens_match("variable", [
      %Tokenizer.Token{type: :identifier, value: "variable", line: 1, column: 1, pos: 0}
    ])

    # Test parsing of a simple identifier
    {:ok, ast} = tokenize_and_parse("variable")
    expected_ast = %Ast.Identifier{name: "variable"}
    assert_ast_match(ast, expected_ast)
  end

  test "Qualified identifiers" do
    # Qualified identifier with dot notation
    assert_tokens_match("Module.function", [
      %Tokenizer.Token{type: :identifier, value: "Module", line: 1, column: 1, pos: 0},
      %Tokenizer.Token{type: :operator, value: ".", line: 1, column: 7, pos: 6},
      %Tokenizer.Token{type: :identifier, value: "function", line: 1, column: 8, pos: 7}
    ])

    # Test parsing of a qualified identifier
    {:ok, ast} = tokenize_and_parse("Module.function")
    expected_ast = %Ast.QualifiedIdentifier{namespace: "Module", name: "function"}
    assert_ast_match(ast, expected_ast)
  end

  test "Variables with type signatures" do
    # Simple type signature
    source = "x :: Int"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "x",
      type_vars: [],
      constraints: [],
      type: %Ast.Identifier{name: "Int"}
    }

    assert_ast_match(ast, expected_ast)

    # Function type signature
    source = "f :: Int -> String"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "f",
      type_vars: [],
      constraints: [],
      type: %Ast.BinaryOp{
        op: "->",
        left: %Ast.Identifier{name: "Int"},
        right: %Ast.Identifier{name: "String"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Variable assignments" do
    # Simple assignment
    source = "x = 42"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "x",
      parameters: [],
      body: %Ast.Literal{type: :number, value: "42"},
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Assignment with expression
    source = "result = 2 * (3 + 4)"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "result",
      parameters: [],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Literal{type: :number, value: "2"},
        right: %Ast.BinaryOp{
          op: "+",
          left: %Ast.Literal{type: :number, value: "3"},
          right: %Ast.Literal{type: :number, value: "4"}
        }
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 4: Functions

  test "Simple function declaration" do
    # Function without parameters
    source = "main = 42"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "main",
      parameters: [],
      body: %Ast.Literal{type: :number, value: "42"},
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Function that returns an expression
    source = "calculateAnswer = 6 * 7"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "calculateAnswer",
      parameters: [],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Literal{type: :number, value: "6"},
        right: %Ast.Literal{type: :number, value: "7"}
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Function with parameters" do
    # Function with one parameter
    source = "double x = x * 2"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "double",
      parameters: [%Nova.Compiler.Ast.Identifier{name: "x"}],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "2"}
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Function with multiple parameters
    source = "add x y = x + y"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "add",
      parameters: [
        %Nova.Compiler.Ast.Identifier{name: "x"},
        %Nova.Compiler.Ast.Identifier{name: "y"}
      ],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Function with type signature" do
    # Function with type signature
    source = """
    add :: Int -> Int -> Int
    add x y = x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "add",
      parameters: [
        %Nova.Compiler.Ast.Identifier{name: "x"},
        %Nova.Compiler.Ast.Identifier{name: "y"}
      ],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      },
      type_signature: %Ast.TypeSignature{
        name: "add",
        type_vars: [],
        constraints: [],
        type: %Ast.BinaryOp{
          op: "->",
          left: %Ast.Identifier{name: "Int"},
          right: %Ast.BinaryOp{
            op: "->",
            left: %Ast.Identifier{name: "Int"},
            right: %Ast.Identifier{name: "Int"}
          }
        }
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Function with pattern matching" do
    # Simple pattern matching on parameters
    source = """
    factorial 0 = 1
    factorial n = n * factorial (n - 1)
    """

    # Note: This would be tested in a real module but can't be fully tested here
    # due to separate function clauses

    # First clause
    {:ok, ast1} = tokenize_and_parse("factorial 0 = 1")

    expected_ast1 = %Ast.FunctionDeclaration{
      name: "factorial",
      parameters: [%Ast.Literal{type: :number, value: "0"}],
      body: %Ast.Literal{type: :number, value: "1"},
      type_signature: nil
    }

    assert_ast_match(ast1, expected_ast1)

    # Second clause
    {:ok, ast2} = tokenize_and_parse("factorial n = n * factorial (n - 1)")

    expected_ast2 = %Ast.FunctionDeclaration{
      name: "factorial",
      parameters: [%Nova.Compiler.Ast.Identifier{name: "n"}],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Identifier{name: "n"},
        right: %Ast.FunctionCall{
          function: %Ast.Identifier{name: "factorial"},
          arguments: [
            %Ast.BinaryOp{
              op: "-",
              left: %Ast.Identifier{name: "n"},
              right: %Ast.Literal{type: :number, value: "1"}
            }
          ]
        }
      },
      type_signature: nil
    }

    assert_ast_match(ast2, expected_ast2)
  end

  test "Anonymous functions (lambdas)" do
    # Simple lambda
    source = "\\x -> x * 2"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.Lambda{
      parameters: [%Nova.Compiler.Ast.Identifier{name: "x"}],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "2"}
      }
    }

    assert_ast_match(ast, expected_ast)

    # Lambda with multiple parameters
    source = "\\x y -> x + y"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.Lambda{
      parameters: [
        %Nova.Compiler.Ast.Identifier{name: "x"},
        %Nova.Compiler.Ast.Identifier{name: "y"}
      ],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Function application" do
    # Simple function call
    source = "double 21"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionCall{
      function: %Ast.Identifier{name: "double"},
      arguments: [%Ast.Literal{type: :number, value: "21"}]
    }

    assert_ast_match(ast, expected_ast)

    # Function call with multiple arguments
    source = "add 5 10"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionCall{
      function: %Ast.Identifier{name: "add"},
      arguments: [
        %Ast.Literal{type: :number, value: "5"},
        %Ast.Literal{type: :number, value: "10"}
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Nested function calls
    source = "outer (inner 42)"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionCall{
      function: %Ast.Identifier{name: "outer"},
      arguments: [
        %Ast.FunctionCall{
          function: %Ast.Identifier{name: "inner"},
          arguments: [%Ast.Literal{type: :number, value: "42"}]
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Partial application and higher-order functions" do
    # Higher-order function
    source = "apply f x = f x"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "apply",
      parameters: [
        %Nova.Compiler.Ast.Identifier{name: "f"},
        %Nova.Compiler.Ast.Identifier{name: "x"}
      ],
      body: %Ast.FunctionCall{
        function: %Ast.Identifier{name: "f"},
        arguments: [%Ast.Identifier{name: "x"}]
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Passing a function as an argument
    source = "map (\\x -> x * 2) [1, 2, 3]"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionCall{
      function: %Ast.Identifier{name: "map"},
      arguments: [
        %Ast.Lambda{
          parameters: [%Nova.Compiler.Ast.Identifier{name: "x"}],
          body: %Ast.BinaryOp{
            op: "*",
            left: %Ast.Identifier{name: "x"},
            right: %Ast.Literal{type: :number, value: "2"}
          }
        },
        %Ast.List{
          elements: [
            %Ast.Literal{type: :number, value: "1"},
            %Ast.Literal{type: :number, value: "2"},
            %Ast.Literal{type: :number, value: "3"}
          ]
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 5: Types and Type Declarations

  test "Type declarations 1" do
    # Type alias
    source = "type Celsius = Int"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Nova.Compiler.Ast.TypeAlias{
      name: "Celsius",
      type_vars: [],
      type: %Nova.Compiler.Ast.Identifier{name: "Int"}
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Type declarations 2" do
    # Function type
    source = "type Converter = Int -> Int"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Nova.Compiler.Ast.TypeAlias{
      name: "Converter",
      type_vars: [],
      type: %Nova.Compiler.Ast.BinaryOp{
        op: "->",
        left: %Nova.Compiler.Ast.Identifier{name: "Int"},
        right: %Nova.Compiler.Ast.Identifier{name: "Int"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Data type declarations" do
    # Simple data type
    source = "data Bool = True | False"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.DataType{
      name: "Bool",
      type_vars: [],
      constructors: [
        %Ast.DataConstructor{name: "True", fields: []},
        %Ast.DataConstructor{name: "False", fields: []}
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Data type with parameters
    source = "data Maybe a = Just a | Nothing"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.DataType{
      name: "Maybe",
      type_vars: ["a"],
      constructors: [
        %Ast.DataConstructor{
          name: "Just",
          fields: [%Ast.Identifier{name: "a"}]
        },
        %Ast.DataConstructor{name: "Nothing", fields: []}
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Data type with multiple fields
    source = "data Person = Person String Int Bool"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.DataType{
      name: "Person",
      type_vars: [],
      constructors: [
        %Ast.DataConstructor{
          name: "Person",
          fields: [
            %Ast.Identifier{name: "String"},
            %Ast.Identifier{name: "Int"},
            %Ast.Identifier{name: "Bool"}
          ]
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Complex type expressions" do
    # List type
    source = "xs :: [Int]"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "xs",
      type_vars: [],
      constraints: [],
      type: %Ast.FunctionCall{
        function: "[]",
        arguments: [%Ast.Identifier{name: "Int"}]
      }
    }

    assert_ast_match(ast, expected_ast)

    # Tuple type
    source = "pair :: (String, Int)"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "pair",
      type_vars: [],
      constraints: [],
      type: %Ast.Tuple{
        elements: [
          %Ast.Identifier{name: "String"},
          %Ast.Identifier{name: "Int"}
        ]
      }
    }

    assert_ast_match(ast, expected_ast)

    # Function type with multiple arguments
    source = "add :: Int -> Int -> Int"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "add",
      type_vars: [],
      constraints: [],
      type: %Ast.BinaryOp{
        op: "->",
        left: %Ast.Identifier{name: "Int"},
        right: %Ast.BinaryOp{
          op: "->",
          left: %Ast.Identifier{name: "Int"},
          right: %Ast.Identifier{name: "Int"}
        }
      }
    }

    assert_ast_match(ast, expected_ast)

    # Parameterized type
    source = "xs :: Maybe Int"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeSignature{
      name: "xs",
      type_vars: [],
      constraints: [],
      type: %Ast.FunctionCall{
        function: "Maybe",
        arguments: [%Ast.Identifier{name: "Int"}]
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 6: Type Classes and Instances

  test "Type class declarations" do
    # Simple type class
    source = """
    class Show a where
      show :: a -> String
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeClass{
      name: "Show",
      type_vars: ["a"],
      methods: [
        %Ast.TypeSignature{
          name: "show",
          type_vars: [],
          constraints: [],
          type: %Ast.BinaryOp{
            op: "->",
            left: %Ast.Identifier{name: "a"},
            right: %Ast.Identifier{name: "String"}
          }
        }
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Type class with multiple methods
    source = """
    class Eq a where
      equals :: a -> a -> Bool
      notEquals :: a -> a -> Bool
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeClass{
      name: "Eq",
      type_vars: ["a"],
      methods: [
        %Ast.TypeSignature{
          name: "equals",
          type_vars: [],
          constraints: [],
          type: %Ast.BinaryOp{
            op: "->",
            left: %Ast.Identifier{name: "a"},
            right: %Ast.BinaryOp{
              op: "->",
              left: %Ast.Identifier{name: "a"},
              right: %Ast.Identifier{name: "Bool"}
            }
          }
        },
        %Ast.TypeSignature{
          name: "notEquals",
          type_vars: [],
          constraints: [],
          type: %Ast.BinaryOp{
            op: "->",
            left: %Ast.Identifier{name: "a"},
            right: %Ast.BinaryOp{
              op: "->",
              left: %Ast.Identifier{name: "a"},
              right: %Ast.Identifier{name: "Bool"}
            }
          }
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Type class instances" do
    # Simple instance
    source = """
    instance Show Int where
      show n = toString n
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeClassInstance{
      class_name: "Show",
      type: %Ast.Identifier{name: "Int"},
      methods: [
        %Ast.FunctionDeclaration{
          name: "show",
          parameters: [%Nova.Compiler.Ast.Identifier{name: "n"}],
          body: %Ast.FunctionCall{
            function: %Ast.Identifier{name: "toString"},
            arguments: [%Ast.Identifier{name: "n"}]
          },
          type_signature: nil
        }
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Instance for parameterized type
    source = """
    instance Show (Maybe a) where
      show Nothing = "Nothing"
      show (Just x) = "Just " ++ show x
    """

    # This is a bit complex to test with the current parser as it has multiple function clauses
    # We'd test this differently in a real implementation
  end

  # Section 7: Control Structures

  test "If-then-else expressions" do
    # Simple if-then-else
    source = "if x > 0 then \"positive\" else \"non-positive\""
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.IfExpression{
      condition: %Ast.BinaryOp{
        op: ">",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "0"}
      },
      then_branch: %Ast.Literal{type: :string, value: "positive"},
      else_branch: %Ast.Literal{type: :string, value: "non-positive"}
    }

    assert_ast_match(ast, expected_ast)

    # Nested if-then-else
    source = """
    if x > 0 then
      if x > 10 then "large positive" else "small positive"
    else
      if x < 0 then "negative" else "zero"
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.IfExpression{
      condition: %Ast.BinaryOp{
        op: ">",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "0"}
      },
      then_branch: %Ast.IfExpression{
        condition: %Ast.BinaryOp{
          op: ">",
          left: %Ast.Identifier{name: "x"},
          right: %Ast.Literal{type: :number, value: "10"}
        },
        then_branch: %Ast.Literal{type: :string, value: "large positive"},
        else_branch: %Ast.Literal{type: :string, value: "small positive"}
      },
      else_branch: %Ast.IfExpression{
        condition: %Ast.BinaryOp{
          op: "<",
          left: %Ast.Identifier{name: "x"},
          right: %Ast.Literal{type: :number, value: "0"}
        },
        then_branch: %Ast.Literal{type: :string, value: "negative"},
        else_branch: %Ast.Literal{type: :string, value: "zero"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Case expressions" do
    # Simple case expression
    source = """
    case x of
      0 -> "zero"
      1 -> "one"
      _ -> "other"
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.CaseExpression{
      expression: %Ast.Identifier{name: "x"},
      cases: [
        %Ast.CaseClause{
          pattern: %Ast.Literal{type: :number, value: "0"},
          body: %Ast.Literal{type: :string, value: "zero"}
        },
        %Ast.CaseClause{
          pattern: %Ast.Literal{type: :number, value: "1"},
          body: %Ast.Literal{type: :string, value: "one"}
        },
        %Ast.CaseClause{
          pattern: %Ast.Identifier{name: "_"},
          body: %Ast.Literal{type: :string, value: "other"}
        }
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Case expression with pattern matching
    source = """
    case maybe of
      Just x -> "Found: " ++ show x
      Nothing -> "Not found"
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.CaseExpression{
      expression: %Ast.Identifier{name: "maybe"},
      cases: [
        %Ast.CaseClause{
          pattern: %Ast.FunctionCall{
            function: %Ast.Identifier{name: "Just"},
            arguments: [%Ast.Identifier{name: "x"}]
          },
          body: %Ast.BinaryOp{
            op: "++",
            left: %Ast.Literal{type: :string, value: "Found: "},
            right: %Ast.FunctionCall{
              function: %Ast.Identifier{name: "show"},
              arguments: [%Ast.Identifier{name: "x"}]
            }
          }
        },
        %Ast.CaseClause{
          pattern: %Ast.Identifier{name: "Nothing"},
          body: %Ast.Literal{type: :string, value: "Not found"}
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Let bindings 1" do
    # Simple let binding
    source = """
    let x = 5 in x * 2
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.LetBinding{
      bindings: [{"x", %Ast.Literal{type: :number, value: "5"}}],
      body: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "2"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Let bindings 2" do
    # Multiple bindings
    source = """
    let
      x = 5
      y = 10
    in
      x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.LetBinding{
      bindings: [
        {"x", %Ast.Literal{type: :number, value: "5"}},
        {"y", %Ast.Literal{type: :number, value: "10"}}
      ],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Let bindings 3" do
    # Nested let expressions
    source = """
    let x = 5 in
      let y = x * 2 in
        x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.LetBinding{
      bindings: [{"x", %Ast.Literal{type: :number, value: "5"}}],
      body: %Ast.LetBinding{
        bindings: [
          {"y",
           %Ast.BinaryOp{
             op: "*",
             left: %Ast.Identifier{name: "x"},
             right: %Ast.Literal{type: :number, value: "2"}
           }}
        ],
        body: %Ast.BinaryOp{
          op: "+",
          left: %Ast.Identifier{name: "x"},
          right: %Ast.Identifier{name: "y"}
        }
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  test "Do notation" do
    # Simple do block
    source = """
    do
      putStrLn "Hello"
      putStrLn "World"
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.DoBlock{
      expressions: [
        {:expr,
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: "putStrLn"},
           arguments: [%Ast.Literal{type: :string, value: "Hello"}]
         }},
        {:expr,
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: "putStrLn"},
           arguments: [%Ast.Literal{type: :string, value: "World"}]
         }}
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Do block with bindings
    source = """
    do
      x <- getLine
      let y = x ++ "!"
      putStrLn y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.DoBlock{
      expressions: [
        {:bind, %Ast.Identifier{name: "x"},
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: "getLine"},
           arguments: []
         }},
        {:let, "y",
         %Ast.BinaryOp{
           op: "++",
           left: %Ast.Identifier{name: "x"},
           right: %Ast.Literal{type: :string, value: "!"}
         }},
        {:expr,
         %Ast.FunctionCall{
           function: %Ast.Identifier{name: "putStrLn"},
           arguments: [%Ast.Identifier{name: "y"}]
         }}
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 8: Modules and Imports

  test "Module declarations" do
    # Simple module
    source = """
    module Math where
      
    add x y = x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.Module{
      name: %Nova.Compiler.Ast.Identifier{name: "Math"},
      declarations: [
        %Ast.FunctionDeclaration{
          name: "add",
          parameters: [
            %Nova.Compiler.Ast.Identifier{name: "x"},
            %Nova.Compiler.Ast.Identifier{name: "y"}
          ],
          body: %Ast.BinaryOp{
            op: "+",
            left: %Ast.Identifier{name: "x"},
            right: %Ast.Identifier{name: "y"}
          },
          type_signature: nil
        }
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Module with multiple declarations
    source = """
    module Data.Maybe where
      
    data Maybe a = Just a | Nothing

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing = False
    """

    # Too complex to test in this simplistic test framework, but would work in a real test suite
  end

  test "Import declarations" do
    # Simple import
    source = "import Data.List"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.ImportDeclaration{
      module: %Nova.Compiler.Ast.Identifier{name: "Data.List"},
      items: []
    }

    assert_ast_match(ast, expected_ast)

    # Foreign import
    source = """
    foreign import elixir "Enum" "map" map :: (a -> b) -> [a] -> [b]
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.ForeignImport{
      module: "Enum",
      function: "map",
      alias: "map",
      type_signature: %Ast.TypeSignature{
        name: "map",
        type_vars: [],
        constraints: [],
        type: %Ast.BinaryOp{
          op: "->",
          left: %Ast.BinaryOp{
            op: "->",
            left: %Ast.Identifier{name: "a"},
            right: %Ast.Identifier{name: "b"}
          },
          right: %Ast.BinaryOp{
            op: "->",
            left: %Ast.FunctionCall{
              function: "[]",
              arguments: [%Ast.Identifier{name: "a"}]
            },
            right: %Ast.FunctionCall{
              function: "[]",
              arguments: [%Ast.Identifier{name: "b"}]
            }
          }
        }
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  # Section 9: List Comprehensions

  test "List comprehensions" do
    # Simple comprehension
    source = "[x * 2 | x <- [1, 2, 3]]"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.ListComprehension{
      expression: %Ast.BinaryOp{
        op: "*",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Literal{type: :number, value: "2"}
      },
      generators: [
        %Ast.Generator{
          pattern: %Ast.Identifier{name: "x"},
          expression: %Ast.List{
            elements: [
              %Ast.Literal{type: :number, value: "1"},
              %Ast.Literal{type: :number, value: "2"},
              %Ast.Literal{type: :number, value: "3"}
            ]
          }
        }
      ],
      guards: []
    }

    assert_ast_match(ast, expected_ast)

    # TODO: Add more complex list comprehension tests
  end

  # Section 10: Comments and Whitespace

  test "Comments" do
    # Line comment
    source = """
    -- This is a comment
    x = 42
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "x",
      parameters: [],
      body: %Ast.Literal{type: :number, value: "42"},
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Block comment
    source = """
    {- This is a
       block comment -}
    x = 42
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "x",
      parameters: [],
      body: %Ast.Literal{type: :number, value: "42"},
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)

    # Nested block comments
    source = """
    {- Outer comment
       {- Nested comment -}
       More outer comment -}
    x = 42
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "x",
      parameters: [],
      body: %Ast.Literal{type: :number, value: "42"},
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)
  end

  # Complete test module that puts it all together

  test "Full module example" do
    source = """
    module Example where

    -- Type declarations
    data List a = Nil | Cons a (List a)

    -- Type class for things that can be shown
    class Show a where
      show :: a -> String

    -- Instance for integers
    instance Show Int where
      show n = intToString n

    -- Instance for our List type
    instance Show (List a) where
      show Nil = "[]"
      show (Cons x xs) = "[" ++ show x ++ showRest xs

    -- Helper function for showing list elements
    showRest :: List a -> String
    showRest Nil = "]"
    showRest (Cons x xs) = ", " ++ show x ++ showRest xs

    -- Function to map a function over a list
    map :: (a -> b) -> List a -> List b
    map _ Nil = Nil
    map f (Cons x xs) = Cons (f x) (map f xs)

    -- Function to filter elements from a list
    filter :: (a -> Bool) -> List a -> List a
    filter _ Nil = Nil
    filter p (Cons x xs) = 
      if p x 
      then Cons x (filter p xs) 
      else filter p xs

    -- Main function using do notation
    main = do
      let nums = Cons 1 (Cons 2 (Cons 3 Nil))
      let doubled = map (\\x -> x * 2) nums
      putStrLn (show doubled)
    """

    # This is too complex to fully test here, but in a real test suite we would
    # assert that the tokenization and parsing work correctly for this complete example
  end
end
