defmodule Nova.TypeChecker.IncrementalTest do
  use ExUnit.Case
  alias Nova.Compiler.{Tokenizer, Parser, TypeChecker}
  alias Nova.Compiler.Env

  """

  module Nova.Compiler.Tokenizer where

  import Prelude

  import Data.List (List(..), (:))
  import Data.Maybe (Maybe(..))
  import Data.String (Pattern(..), charAt, drop, length, singleton, splitAt, take, uncons)
  import Data.String.CodeUnits (fromCharArray, toCharArray)
  import Data.Tuple (Tuple(..))

  type Position = 
  { line :: Int
  , column :: Int
  , pos :: Int
  }

  data TokenType
  = Keyword
  | Identifier
  | Operator
  | Number
  | String
  | Char
  | Delimiter
  | Unrecognized

  type Token =
  { type :: TokenType
  , value :: String
  , line :: Int
  , column :: Int
  , pos :: Int
  }

  keywords :: Array String
  keywords = 
  [ "module", "where", "import", "data", "type", "class", "instance"
  , "let", "in", "if", "then", "else", "case", "of", "do"
  ]

  operators :: Array String
  operators =
  [ "+", "-", "*", "/", "=", "==", "!=", "<", "<=", ">", ">="
  , "->", "=>", ":", "::", ".", "|", "\\", "&&", "||"
  ]

  tokenize :: String -> List Token
  tokenize source = tokenize_ source Nil initialPosition
  where
    initialPosition = { line: 1, column: 1, pos: 0 }

  tokenize_ :: String -> List Token -> Position -> List Token
  tokenize_ "" tokens _ = tokens
  tokenize_ source tokens pos =
  case uncons $ toCharArray source of
    Nothing -> tokens
    Just { head, tail } -> 
      let restStr = fromCharArray tail in
      case head of
        '\n' -> 
          let newPos = { line: pos.line + 1, column: 1, pos: pos.pos + 1 }
          in tokenize_ restStr tokens newPos
          
        ' ' -> 
          let newPos = { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
          in tokenize_ restStr tokens newPos
          
        '\t' -> 
          let newPos = { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
          in tokenize_ restStr tokens newPos
          
        _ ->
          if take 2 source == "--" then
            let { newSource, newPos } = consumeLineComment (drop 2 source) 
                  { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            in tokenize_ newSource tokens newPos
            
          else if take 2 source == "{-" then
            let { newSource, newPos } = consumeBlockComment (drop 2 source) 
                  { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 } 1
            in tokenize_ newSource tokens newPos
            
          else if head == '"' then
            let { value, newSource, newPos } = consumeString restStr "" 
                  { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
                token = { type: String, value, line: pos.line, column: pos.column, pos: pos.pos }
            in tokenize_ newSource (token : tokens) 
                { line: newPos.line, column: newPos.column + 1, pos: newPos.pos + 1 }
                
          else if head == '\'' then
            let { value, newSource, newPos } = consumeChar restStr 
                  { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
                token = { type: Char, value, line: pos.line, column: pos.column, pos: pos.pos }
            in tokenize_ newSource (token : tokens) 
                { line: newPos.line, column: newPos.column + 1, pos: newPos.pos + 1 }
                
          else if isDigit head then
            let { value, newSource, newPos } = consumeNumber source "" pos
                token = { type: Number, value, line: pos.line, column: pos.column, pos: pos.pos }
            in tokenize_ newSource (token : tokens) newPos
            
          else if isOperatorChar head then
            let { value, newSource, newPos } = consumeOperator source pos
                token = { type: Operator, value, line: pos.line, column: pos.column, pos: pos.pos }
            in tokenize_ newSource (token : tokens) newPos
            
          else if isIdentStart head then
            let { value, newSource, newPos } = consumeIdentifier source "" pos
                tokenType = if isKeyword value then Keyword else Identifier
                token = { type: tokenType, value, line: pos.line, column: pos.column, pos: pos.pos }
            in tokenize_ newSource (token : tokens) newPos
            
          else if isDelimiter head then
            let token = { type: Delimiter, value: singleton head, line: pos.line, column: pos.column, pos: pos.pos }
                newPos = { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
            in tokenize_ restStr (token : tokens) newPos
            
          else
            let token = { type: Unrecognized, value: singleton head, line: pos.line, column: pos.column, pos: pos.pos }
                newPos = { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
            in tokenize_ restStr (token : tokens) newPos

  consumeLineComment :: String -> Position -> { newSource :: String, newPos :: Position }
  consumeLineComment source pos =
  case uncons $ toCharArray source of
    Nothing -> { newSource: "", newPos: pos }
    Just { head, tail } ->
      if head == '\n' then
        { newSource: source, newPos: pos }
      else
        consumeLineComment (fromCharArray tail) 
          { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }

  consumeBlockComment :: String -> Position -> Int -> { newSource :: String, newPos :: Position }
  consumeBlockComment source pos depth =
  if depth == 0 then
    { newSource: source, newPos: pos }
  else if take 2 source == "-}" && depth == 1 then
    { newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "{-" then
    consumeBlockComment (drop 2 source) 
      { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 } (depth + 1)
  else if take 2 source == "-}" then
    consumeBlockComment (drop 2 source)
      { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 } (depth - 1)
  else
    case uncons $ toCharArray source of
      Nothing -> { newSource: "", newPos: pos }
      Just { head, tail } ->
        if head == '\n' then
          consumeBlockComment (fromCharArray tail)
            { line: pos.line + 1, column: 1, pos: pos.pos + 1 } depth
        else
          consumeBlockComment (fromCharArray tail)
            { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 } depth

  consumeString :: String -> String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeString source acc pos =
  case uncons $ toCharArray source of
    Nothing -> { value: acc, newSource: "", newPos: pos }
    Just { head, tail } ->
      let restStr = fromCharArray tail in
      if head == '"' then
        { value: acc
        , newSource: restStr
        , newPos: { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
        }
      else if head == '\\' && length restStr > 0 then
        case charAt 0 restStr of
          Just '\"' -> 
            consumeString (drop 1 restStr) (acc <> "\\\"")
              { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
          Just 'n' ->
            consumeString (drop 1 restStr) (acc <> "\\n")
              { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
          Just 't' ->
            consumeString (drop 1 restStr) (acc <> "\\t")
              { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
          Just 'r' ->
            consumeString (drop 1 restStr) (acc <> "\\r")
              { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
          Just '\\' ->
            consumeString (drop 1 restStr) (acc <> "\\\\")
              { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
          _ ->
            consumeString restStr (acc <> singleton head)
              { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
      else if head == '\n' then
        consumeString restStr (acc <> singleton head)
          { line: pos.line + 1, column: 1, pos: pos.pos + 1 }
      else
        consumeString restStr (acc <> singleton head)
          { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }

  consumeChar :: String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeChar source pos =
  case uncons $ toCharArray source of
    Nothing -> { value: "", newSource: "", newPos: pos }
    Just { head, tail } ->
      let restStr = fromCharArray tail in
      if head == '\'' then
        { value: ""
        , newSource: restStr
        , newPos: { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
        }
      else if head == '\\' && length restStr > 0 then
        case charAt 0 restStr of
          Just '\'' -> 
            { value: "'"
            , newSource: drop 1 restStr
            , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            }
          Just 'n' ->
            { value: "\n"
            , newSource: drop 1 restStr
            , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            }
          Just 't' ->
            { value: "\t"
            , newSource: drop 1 restStr
            , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            }
          Just 'r' ->
            { value: "\r"
            , newSource: drop 1 restStr
            , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            }
          Just '\\' ->
            { value: "\\"
            , newSource: drop 1 restStr
            , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
            }
          _ ->
            { value: singleton head
            , newSource: restStr
            , newPos: { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
            }
      else
        { value: singleton head
        , newSource: restStr
        , newPos: { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
        }

  consumeNumber :: String -> String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeNumber source acc pos =
  case uncons $ toCharArray source of
    Nothing -> { value: acc, newSource: "", newPos: pos }
    Just { head, tail } ->
      let restStr = fromCharArray tail in
      if isDigit head then
        consumeNumber restStr (acc <> singleton head)
          { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
      else if head == '.' && length restStr > 0 && isDigit (charAt 0 restStr) then
        consumeNumber (drop 1 restStr) (acc <> "." <> singleton (charAt 0 restStr))
          { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
      else
        { value: acc
        , newSource: source
        , newPos: { line: pos.line, column: pos.column, pos: pos.pos }
        }

  consumeOperator :: String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeOperator source pos =
  if take 2 source == "==" then
    { value: "=="
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "!=" then
    { value: "!="
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "<=" then
    { value: "<="
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == ">=" then
    { value: ">="
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "->" then
    { value: "->"
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "=>" then
    { value: "=>"
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "::" then
    { value: "::"
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "&&" then
    { value: "&&"
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else if take 2 source == "||" then
    { value: "||"
    , newSource: drop 2 source
    , newPos: { line: pos.line, column: pos.column + 2, pos: pos.pos + 2 }
    }
  else
    case uncons $ toCharArray source of
      Nothing -> { value: "", newSource: "", newPos: pos }
      Just { head, tail } ->
        if isOperatorChar head then
          { value: singleton head
          , newSource: fromCharArray tail
          , newPos: { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
          }
        else
          { value: ""
          , newSource: source
          , newPos: pos
          }

  consumeIdentifier :: String -> String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeIdentifier source acc pos =
  case uncons $ toCharArray source of
    Nothing -> { value: acc, newSource: "", newPos: pos }
    Just { head, tail } ->
      if isIdentChar head then
        consumeIdentifier (fromCharArray tail) (acc <> singleton head) { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
      else
        { value: acc
        , newSource: source
        , newPos: { line: pos.line, column: pos.column, pos: pos.pos }
        }

  isDigit :: Char -> Boolean
  isDigit c = c >= '0' && c <= '9'

  isLetter :: Char -> Boolean
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  isIdentStart :: Char -> Boolean
  isIdentStart c = isLetter c || c == '_'

  isIdentChar :: Char -> Boolean
  isIdentChar c = isLetter c || isDigit c || c == '_'

  isOperatorChar :: Char -> Boolean
  isOperatorChar c = c `elem` ['+', '-', '*', '/', '=', '<', '>', ':', '.', '|', '\\', '&']

  isDelimiter :: Char -> Boolean
  isDelimiter c = c `elem` ['(', ')', '{', '}', '[', ']', ',', ';']

  isKeyword :: String -> Boolean
  isKeyword str = str `elem` keywords

  elem :: forall a. Eq a => a -> Array a -> Boolean
  elem x xs = case indexOf x xs of
  Just _ -> true
  Nothing -> false

  indexOf :: forall a. Eq a => a -> Array a -> Maybe Int
  indexOf = indexOf' 0
  where
    indexOf' _ _ [] = Nothing
    indexOf' i x (y : ys) = if x == y then Just i else indexOf' (i + 1) x ys
  """

  @source """

  isDigit :: Char -> Boolean
  isDigit c = c >= '0' && c <= '9'

  isLetter :: Char -> Boolean
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  isIdentStart :: Char -> Boolean
  isIdentStart c = isLetter c || c == '_'

  isIdentChar :: Char -> Boolean
  isIdentChar c = isLetter c || isDigit c || c == '_'

  isOperatorChar :: Char -> Boolean
  isOperatorChar c = c `elem` ['+', '-', '*', '/', '=', '<', '>', ':', '.', '|', '\\\\', '&']

  isDelimiter :: Char -> Boolean
  isDelimiter c = c `elem` ['(', ')', '{', '}', '[', ']', ',', ';']

  consumeIdentifier :: String -> String -> Position -> { value :: String, newSource :: String, newPos :: Position }
  consumeIdentifier source acc pos =
  case uncons $ toCharArray source of
    Nothing -> { value: acc, newSource: "", newPos: pos }
    Just { head, tail } ->
      if isIdentChar head then
        consumeIdentifier (fromCharArray tail) (acc <> singleton head) { line: pos.line, column: pos.column + 1, pos: pos.pos + 1 }
      else
        { value: acc
        , newSource: source
        , newPos: { line: pos.line, column: pos.column, pos: pos.pos }
        }

  isKeyword :: String -> Boolean
  isKeyword str = str `elem` keywords



  """

  test "incrementally adds each declaration to the environment" do
    IO.puts(@source)
    tokens = IO.inspect(Tokenizer.tokenize(@source))
    {:ok, decls, rest} = IO.inspect(Parser.parse_declarations(tokens))
    assert rest == []
    IO.inspect(decls)
    IO.inspect(rest)
    # {:ok, %Nova.Compiler.Ast.Module{body: decls}} = Parser.parse(tokens)

    # Start with empty type environment
    Enum.reduce(decls, Env.empty(), fn decl, env ->
      case TypeChecker.check_declaration(decl, env) do
        {:ok, _type, _subst, updated_env} ->
          updated_env

        {:error, reason} ->
          flunk("Type error in declaration #{inspect(decl)}: #{inspect(reason)}")
      end
    end)
  end
end
