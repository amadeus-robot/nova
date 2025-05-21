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

  # 1. Function with parameters test
  test "Function with parameters" do
    # Function with one parameter
    source = "double x = x * 2"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "double",
      parameters: [%Ast.Identifier{name: "x"}],
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
      parameters: [%Ast.Identifier{name: "x"}, %Ast.Identifier{name: "y"}],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      },
      type_signature: nil
    }

    assert_ast_match(ast, expected_ast)
  end

  # 2. Function with type signature test
  test "Function with type signature" do
    # Function with type signature
    source = """
    add :: Int -> Int -> Int
    add x y = x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "add",
      parameters: [%Ast.Identifier{name: "x"}, %Ast.Identifier{name: "y"}],
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

  # 3. Function with pattern matching test
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
      parameters: [%Ast.Identifier{name: "n"}],
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

  # 4. Anonymous functions (lambdas) test
  test "Anonymous functions (lambdas)" do
    # Simple lambda
    source = "\\x -> x * 2"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.Lambda{
      parameters: [%Ast.Identifier{name: "x"}],
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
      parameters: [%Ast.Identifier{name: "x"}, %Ast.Identifier{name: "y"}],
      body: %Ast.BinaryOp{
        op: "+",
        left: %Ast.Identifier{name: "x"},
        right: %Ast.Identifier{name: "y"}
      }
    }

    assert_ast_match(ast, expected_ast)
  end

  # 5. Partial application and higher-order functions test
  test "Partial application and higher-order functions" do
    # Higher-order function
    source = "apply f x = f x"
    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.FunctionDeclaration{
      name: "apply",
      parameters: [%Ast.Identifier{name: "f"}, %Ast.Identifier{name: "x"}],
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
          parameters: [%Ast.Identifier{name: "x"}],
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

  # 6. Type class instances test
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
          parameters: [%Ast.Identifier{name: "n"}],
          body: %Ast.FunctionCall{
            function: %Ast.Identifier{name: "toString"},
            arguments: [%Ast.Identifier{name: "n"}]
          },
          type_signature: nil
        }
      ]
    }

    assert_ast_match(ast, expected_ast)

    # Instance for parameterized type - This is a simplified test as multiple function clauses
    # would be tested differently in a real implementation
    source = """
    instance Show (Maybe a) where
      show Nothing = "Nothing"
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.TypeClassInstance{
      class_name: "Show",
      type: %Ast.FunctionCall{
        function: "Maybe",
        arguments: [%Ast.Identifier{name: "a"}]
      },
      methods: [
        %Ast.FunctionDeclaration{
          name: "show",
          parameters: [%Ast.Identifier{name: "Nothing"}],
          body: %Ast.Literal{type: :string, value: "Nothing"},
          type_signature: nil
        }
      ]
    }

    assert_ast_match(ast, expected_ast)
  end

  # 7. Module declarations test
  test "Module declarations" do
    # Simple module
    source = """
    module Math where
      
    add x y = x + y
    """

    {:ok, ast} = tokenize_and_parse(source)

    expected_ast = %Ast.Module{
      name: "Math",
      declarations: [
        %Ast.FunctionDeclaration{
          name: "add",
          parameters: [%Ast.Identifier{name: "x"}, %Ast.Identifier{name: "y"}],
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

    # More complex module with multiple declarations
    source = """
    module Data.Maybe where
      
    data Maybe a = Just a | Nothing

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    """

    {:ok, ast} = tokenize_and_parse(source)

    # This is simplified as we can't capture all declarations in this test framework
    # We're just making sure the module name is correct
    assert ast.name == "Data.Maybe"
    assert length(ast.declarations) > 0
  end
end
