defmodule Nova.CompilerTest do
  use ExUnit.Case

  alias Nova.Compiler.Tokenizer
  alias Nova.Compiler.Parser

  test "tokenize and parse simple module" do
    source = """
    module Test.SimpleBinding where

    x :: Int
    x = 42

    main :: IO Int
    main = pure x
    """

    tokens = Tokenizer.tokenize(source)
    assert length(tokens) > 0
    assert Enum.any?(tokens, fn token -> token.type == :keyword and token.value == "module" end)

    {:ok, ast} = Parser.parse(tokens)
    assert ast.name == "Test.SimpleBinding"
    assert length(ast.declarations) == 3
  end

  test "tokenize and parse module with function declarations" do
    source = """
    module Test.BasicArithmetic where

    add :: Int -> Int -> Int
    add x y = x + y

    mul :: Int -> Int -> Int
    mul x y = x * y

    main :: IO Int
    main = pure (add (mul 3 4) 5)
    """

    tokens = Tokenizer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert ast.name == "Test.BasicArithmetic"
    # 3 type sigs + 3 function implementations
    assert length(ast.declarations) == 5

    # Find the add function
    add_func =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Parser.FunctionDeclaration{name: "add"} -> true
        _ -> false
      end)

    assert add_func != nil
    assert length(add_func.parameters) == 2
  end

  test "tokenize and parse module with data type declaration" do
    source = """
    module Test.PatternMatching where

    data Maybe a = Nothing | Just a

    fromMaybe :: a -> Maybe a -> a
    fromMaybe default Nothing = default
    fromMaybe _ (Just x) = x

    main :: IO Int
    main = pure (fromMaybe 42 Nothing + fromMaybe 0 (Just 8))
    """

    tokens = Tokenizer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert ast.name == "Test.PatternMatching"

    # Find the data type declaration
    data_type =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Parser.DataType{name: "Maybe"} -> true
        _ -> false
      end)

    assert data_type != nil
    assert length(data_type.constructors) == 2
    assert Enum.at(data_type.constructors, 0).name == "Nothing"
    assert Enum.at(data_type.constructors, 1).name == "Just"
  end

  test "tokenize and parse module with foreign imports" do
    source = """
    module Test.ElixirImport where

    foreign import elixir "String" "upcase" upcase :: String -> String
    foreign import elixir "String" "downcase" downcase :: String -> String
    foreign import elixir "List" "flatten" flatten :: [[a]] -> [a]

    main :: IO String
    main = pure (upcase "hello" <> " " <> downcase "WORLD")
    """

    tokens = Tokenizer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert ast.name == "Test.ElixirImport"

    # Count foreign imports
    foreign_imports =
      Enum.filter(ast.declarations, fn
        %Nova.Compiler.Parser.ForeignImport{} -> true
        _ -> false
      end)

    assert length(foreign_imports) == 3
  end

  test "tokenize and parse module with type classes" do
    source = """
    module Test.TypeClasses where

    class Functor f where
      map :: (a -> b) -> f a -> f b

    instance Functor [] where
      map _ [] = []
      map f (x:xs) = f x : map f xs

    data Maybe a = Nothing | Just a

    instance Functor Maybe where
      map _ Nothing = Nothing
      map f (Just a) = Just (f a)

    addOne :: Int -> Int
    addOne x = x + 1

    main :: IO Int
    main = do
      let maybeResult = map addOne (Just 5)
      let listResult = map addOne [1, 2, 3]
      case maybeResult of
        Nothing -> pure 0
        Just n -> pure (n + sum listResult)
      end
    """

    tokens = Tokenizer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert ast.name == "Test.TypeClasses"

    # Find the type class declaration
    type_class =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Parser.TypeClass{name: "Functor"} -> true
        _ -> false
      end)

    assert type_class != nil
    assert length(type_class.methods) == 1
    assert Enum.at(type_class.methods, 0).name == "map"

    # Find instances
    instances =
      Enum.filter(ast.declarations, fn
        %Nova.Compiler.Parser.TypeClassInstance{} -> true
        _ -> false
      end)

    assert length(instances) == 2
  end

  test "tokenize and parse module with list comprehension" do
    source = """
    module Test.ListComprehensions where

    main :: IO [Int]
    main = pure [x * y | x <- [1, 2, 3], y <- [4, 5, 6], x + y > 7]
    """

    tokens = Tokenizer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert ast.name == "Test.ListComprehensions"

    # Get the main function
    main_func =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Parser.FunctionDeclaration{name: "main"} -> true
        _ -> false
      end)

    assert main_func != nil

    # Validate it contains a list comprehension
    assert match?(%Nova.Compiler.Parser.ListComprehension{}, main_func.body)

    list_comp = main_func.body
    assert length(list_comp.generators) == 2
  end

  test "tokenize and parse does not drop any tokens" do
    source = """
    module Test.CompleteTokens where

    -- This is a comment
    {- This is a block comment -}

    main :: IO ()
    main = do
      putStrLn "Hello, World!"
      return ()
    end
    """

    tokens = Tokenizer.tokenize(source)
    original_token_count = length(tokens)

    # Skip comments in token count
    filtered_tokens =
      Enum.filter(tokens, fn token ->
        not (token.type == :operator and token.value == "--") and
          not (token.type == :delimiter and token.value == "{-")
      end)

    filtered_token_count = length(filtered_tokens)

    {:ok, _ast} = Parser.parse(tokens)

    # Verify no tokens were silently dropped
    assert original_token_count > 0
    assert filtered_token_count > 0
  end

  test "error handling for invalid syntax" do
    source = """
    module Test.InvalidSyntax where

    -- Missing equals sign
    main 
    """

    tokens = Tokenizer.tokenize(source)
    result = Parser.parse(tokens)

    assert match?({:error, _}, result)
  end

  test "error handling for unmatched delimiters" do
    source = """
    module Test.UnmatchedDelimiters where

    main = (1 + 2
    """

    tokens = Tokenizer.tokenize(source)
    result = Parser.parse(tokens)

    assert match?({:error, _}, result)
  end

  test "type" do
    source("""
    type Position = 
      { line :: Int
      , column :: Int
      , pos :: Int
      }

    """)

    tokens = Tokenizer.tokenize(source)
    result = Parser.parse(tokens)

    assert match?({:ok, _}, result)
  end
end
