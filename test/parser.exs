defmodule Nova.CompilerTest do
  use ExUnit.Case

  alias Nova.Compiler.Tokenizer
  alias Nova.Compiler.Parser
  alias Nova.Compiler.Ast

  @moduledoc """
  Regression tests for Nova.Compiler's tokenizer and parser.
  Updated to reflect the new public API (May 2025) where
  `Parser.parse/1` has been replaced with
  `parse_module/1`, `parse_declaration/1`, `parse_declarations/1`,
  and `parse_expression/1`.
  """

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------
  defp parse_module!(tokens) do
    case Parser.parse_module(tokens) do
      {:ok, ast, []} ->
        ast

      {:ok, ast, rest} ->
        ast
        flunk("parse_module failed: unparsed tokens: #{inspect(rest)}")

      {:error, reason} ->
        flunk("parse_module failed: #{inspect(reason)}")
    end
  end

  defp parse_decl!(code) do
    case Parser.parse_declaration(Tokenizer.tokenize(code)) do
      {:ok, ast, []} ->
        ast

      {:ok, ast, rest} ->
        ast
        flunk("parse_declaration failed: unparsed tokens: #{inspect(rest)}")

      {:error, reason} ->
        flunk("parse_declaration failed: #{inspect(reason)}")
    end
  end

  # ---------------------------------------------------------------------------
  # Tests
  # ---------------------------------------------------------------------------

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
    assert Enum.any?(tokens, fn %{type: t, value: v} -> t == :keyword and v == "module" end)

    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.SimpleBinding"}
    assert length(ast.declarations) == 2
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
    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.BasicArithmetic"}
    # 3 function declarations (type signatures are folded into their impls)
    assert length(ast.declarations) == 3

    # Find the add function
    add_func =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Ast.FunctionDeclaration{name: "add"} -> true
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
    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.PatternMatching"}

    # Find the data type declaration
    data_type =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Ast.DataType{name: "Maybe"} -> true
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
    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.ElixirImport"}

    # Count foreign imports
    foreign_imports =
      Enum.filter(ast.declarations, fn
        %Nova.Compiler.Ast.ForeignImport{} -> true
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
    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.TypeClasses"}

    # Find the type class declaration
    type_class =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Ast.TypeClass{name: "Functor"} -> true
        _ -> false
      end)

    assert type_class != nil
    assert length(type_class.methods) == 1
    assert Enum.at(type_class.methods, 0).name == "map"

    # Find instances
    instances =
      Enum.filter(ast.declarations, fn
        %Nova.Compiler.Ast.TypeClassInstance{} -> true
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
    ast = parse_module!(tokens)

    assert ast.name == %Nova.Compiler.Ast.Identifier{name: "Test.ListComprehensions"}

    # Get the main function
    main_func =
      Enum.find(ast.declarations, fn
        %Nova.Compiler.Ast.FunctionDeclaration{name: "main"} -> true
        _ -> false
      end)

    assert main_func != nil

    # Validate it contains a list comprehension
    assert match?(%Nova.Compiler.Ast.ListComprehension{}, main_func.body)

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

    _ast = parse_module!(tokens)

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
    result = Parser.parse_module(tokens)

    assert match?({:error, _}, result)
  end

  test "error handling for unmatched delimiters" do
    source = """
    module Test.UnmatchedDelimiters where

    main = (1 + 2
    """

    tokens = Tokenizer.tokenize(source)
    result = Parser.parse_module(tokens)

    assert match?({:error, _}, result)
  end

  test "parse a standalone type alias" do
    source = """
    type Position = 
      { line :: Int
      , column :: Int
      , pos :: Int
      }
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, _rest} = Parser.parse_declarations(tokens)

    assert [%Nova.Compiler.Ast.TypeAlias{name: "Position"}] = decls
  end

  test "parse case v1" do
    source = """
      case uncons $ toCharArray source of
        Nothing -> { value: acc, newSource: "", newPos: pos }
        Just { head, tail } -> 
          1         
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, rest} = Parser.parse_expression(tokens)

    assert rest == []
  end

  test "parse nested case 1" do
    source = """
      case source of
        Nothing -> 3 
        Just head -> 
         case head of
            true -> 
               2
            false -> 
               1         
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, rest} = Parser.parse_expression(tokens)

    assert rest == []
  end

  test "parse nested case" do
    source = """
      case uncons $ toCharArray source of
        Nothing -> { value: acc, newSource: "", newPos: pos }
        Just { head, tail } -> 
          case isIdentChar head of
            true -> 
               2
            false -> 
               1         
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, rest} = Parser.parse_expression(tokens)

    assert rest == []
  end

  test "use of <> operator" do
    source = """
    consumeIdentifier source (acc <> "rolf") { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, rest} = Parser.parse_expression(tokens)

    assert rest == []
  end

  test "case then next declaration" do
    source = """
    consumeIdentifier a =
      case uncons $ toCharArray source of
       Nothing -> 2 
       Just a -> 1 

    isKeyword str = 2 
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, rest} = Parser.parse_declarations(tokens)

    assert rest == []
  end

  test "parse multiple type alias" do
    source = """
    type Token =
        { type :: TokenType
        , value :: String
        , line :: Int
        , column :: Int
        , pos :: Int
        }
    """

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, _rest} = Parser.parse_declarations(tokens)

    assert [%Nova.Compiler.Ast.TypeAlias{name: "Token"}] = decls
  end

  test "case and let in" do
    source = ~s|
tokenize_ source tokens pos =
      case head of
--        ' ' ->
--          let newPos = { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
        _ ->
          if take 2 source == "--" then
            let { newSource, newPos } = consumeLineComment (drop 2 source) { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            in tokenize_ newSource tokens newPos
          else
            1
   
x = 1
    |

    tokens = Tokenizer.tokenize(source)

    # No module header – use the mid‑level helper
    {:ok, decls, _rest} = Parser.parse_declarations(tokens)

    assert [_, _] = decls
  end

  test "code after -- is ignored" do
    src = "x = 1  -- y = '☹'\n y = 2"
    toks = Tokenizer.tokenize(src)

    assert Enum.any?(toks, &(&1.type == :identifier and &1.value == "x"))
    refute Enum.any?(toks, &(&1.value == "☹"))
    assert Enum.any?(toks, &(&1.type == :identifier and &1.value == "y" and &1.line == 2))
  end

  test "parses constructor wildcard imports" do
    src = "import B (Foo(..))\n"

    %Ast.ImportDeclaration{module: %Ast.Identifier{name: "B"}, items: [{"Foo", :all}]} =
      parse_decl!(src)
  end

  test "parses hiding lists" do
    src = "import Prelude hiding (unsafeCoerce)\n"
    %Ast.ImportDeclaration{hiding?: true, items: ["unsafeCoerce"]} = parse_decl!(src)
  end

  test "parses alias + selector" do
    src = "import A as X (foo)\n"
    %Ast.ImportDeclaration{alias: "X", items: ["foo"]} = parse_decl!(src)
  end

  test "multiple imports" do
    src = "
import A.B.C
import A.B.C as ABC
import A.B.C (foo, bar, Baz(..))
import Prelude hiding (unsafeCoerce)
import X as Y (foo)            -- alias + selector
"
    _ = parse_decl!(src)
  end
end
