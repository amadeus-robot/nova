defmodule WorkFlow.Step1 do
  defmodule Gemini.Client do
    @moduledoc """
    Minimal wrapper around Google Gemini `streamGenerateContent`.
    """

    def start do
      Finch.start_link(name: MyFinch)
    end

    @endpoint "https://generativelanguage.googleapis.com/v1beta/models"
    @model_id "gemini-2.5-flash-preview-04-17"

    @spec generate!(String.t(), keyword()) :: :ok | no_return()
    def generate!(prompt, config \\ []) do
      key = System.fetch_env!("GEMINI_API_KEY")

      body = %{
        contents: [
          %{
            role: "user",
            parts: [%{text: prompt}]
          }
        ],
        generationConfig: %{
          temperature: Keyword.get(config, :temperature, 0.5),
          thinkingConfig: %{
            thinkingBudget: 0
          },
          responseMimeType: "text/plain"
        }
      }

      url = "#{@endpoint}/#{@model_id}:generateContent?key=#{key}"

      # 3. POST and stream chunks straight to STDOUT
      # Build the request
      request = Finch.build(:post, url, [], JSON.encode!(body))

      # Execute the request with a 120-second timeout
      case Finch.request(request, MyFinch, receive_timeout: 120_000) do
        {:ok, response} ->
          IO.puts("Status: #{response.status}")
          json = JSON.decode!(response.body)
          IO.inspect(json, pretty: true, limit: 9_999_999)
          {:ok, json}

        {:error, reason} ->
          IO.puts("Error: #{inspect(reason)}")
          {:error, reason}
      end
    end
  end

  def go(funcs) do
    Enum.each(funcs, fn func ->
      res =
        try do
          p = WorkFlow.Step2.CustomXmlParser.parse(File.read!("wip/imps/#{func.name}"))

          res =
            Nova.Compiler.Tokenizer.tokenize(p.code) |> Nova.Compiler.Parser.parse_declarations()

          case res do
            {:ok, _, []} ->
              :ok

            _ ->
              :error
          end
        catch
          _, _ -> :crash
        end

      if res != :ok do
        prompt = render_prompt(func)
        Gemini.Client.start()
        {:ok, gen} = Gemini.Client.generate!(prompt)
        [%{"content" => %{"parts" => [%{"text" => t}]}}] = gen["candidates"]
        IO.inspect(t)
        File.write("wip/imps/#{func.name}", t)

        raise OneIsEnough
      end
    end)
  end

  def render_prompt(func) do
    """
    this is for namespace Nova.Compiler.Ast
    write in nova, a dialect of purescript that runs in beam
    implement the functions provided in <function> tag
    for each return:
    ```
    <implementation>
     <name>...</name>
     <code>
    nova code...
     </code>
     <tests>
      <test>...</test>
      <test>...</test>
     </tests>
    </implementation>
    ```
    tests will be evaluated as an expression, should return true if success
    all pure data types derive Eq so can be tested for equality

    implementation code doesn't have to redefine or re import things already provided in types or defined_imports section, as these are already available on the namespace, same for tests

    nova rules:
      don't use the composition (<<<) operator
      don't add comments
      for deconstructing arrays head and tail, prefer the syntax 
        ```
        case xs of
           [] -> ...
           h : t -> ...
        ```

      put closing delimiters on their own line
      dont use anonymous functions, use subfunctions with `let` or `where`
        ```
        let 
            f x = x + 1
          in
            f 2
        ```
      follow haskell/purescript layout when using `where`, example:
        ```
         bar 
          where
            bar = 1 
        ```


    <defined_imports>
    foreign import unsafeInspect :: forall a. a -> String -- For debugging, maps to Elixir's inspect

    foreign import elixirEnumDropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
    foreign import elixirEnumReject :: forall a. (a -> Boolean) -> Array a -> Array a
    foreign import elixirEnumReverse :: forall a. Array a -> Array a
    foreign import elixirEnumMap :: forall a b. (a -> b) -> Array a -> Array b
    -- Elixir's Enum.split_while/2 returns a {List.t(), List.t()} tuple (or map in some contexts)
    -- Purescript's Array.span returns Tuple (Array a) (Array a)
    -- Assuming FFI can map to this Tuple structure or a record { before :: Array a, after :: Array a }
    foreign import elixirEnumSplitWhile :: forall a. (a -> Boolean) -> Array a -> Tuple (Array a) (Array a)
    foreign import elixirEnumFindIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
    foreign import elixirEnumDrop :: forall a. Int -> Array a -> Array a
    foreign import elixirListFirst :: forall a. Array a -> Maybe a -- Elixir List.first/1 -> nil | term
    foreign import elixirHd :: forall a. Array a -> Maybe a        -- Erlang hd/1 (on non-empty) or Elixir hd/1
    foreign import elixirLength :: forall a. Array a -> Int       -- Elixir length/1
    foreign import elixirEnumJoin :: Array String -> String -> String -- Elixir Enum.join/2
    </defined_imports>

    <types>
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
       , "->", "=>", ":", "::", ".", "|", "\", "&&", "||"
       ]

    module Nova.Compiler.Ast where

    data Tuple a b = Tuple a b
    data Maybe a = Just a | Nothing
    data QualifiedIdentifier = String
    data Array a = Array a

    type Position =
      { line :: Int
      , column :: Int
      , pos :: Int
      }

    data TokenType = TTKeyword
      | TTIdentifier
      | TTOperator
      | TTNumber
      | TTString
      | TTChar
      | TTDelimiter
      | TTUnrecognized
      | TTNewline

    type Token =
      { ttype :: TokenType
      , value :: String
      , line :: Int
      , column :: Int
      , pos :: Int
      }

    -- Result type for parser functions
    data ParseOutput a = Ok a (Array Token) | Error String

    -- AST Node Types (defined as they are needed)

    data Expression
      = ELambda { parameters :: Array Pattern, body :: Expression }
      | ELetBinding { bindings :: Array (Tuple Pattern Expression), body :: Expression }
      | EIfExpression { condition :: Expression, thenBranch :: Expression, elseBranch :: Expression }
      | ECaseExpression { expression :: Expression, cases :: Array CaseClause }
      | EDoBlock { expressions :: Array DoStatement }
      | EBinaryOp { op :: String, left :: Expression, right :: Expression }
      | EApplication { func :: Expression, arguments :: Array Expression } -- Replaces generic FunctionCall for expressions
      | ERecordLiteral { fields :: Array (Tuple String Expression) }
      | ELiteral LiteralValue
      | EList { elements :: Array Expression }
      | EListComprehension { expression :: Expression, generators :: Array Generator, guards :: Array Expression } -- guards added
      | ETuple { elements :: Array Expression }
      | EIdentifier String -- Simple identifier
      | EQualifiedIdentifier (Array String) -- Path like Mod.Sub.name
      | EParenthesized Expression -- For ((expr))

    data LiteralValue
      = LNumber String
      | LString String
      | LChar String

    data Pattern
      = PRecordPattern { fields :: Array (Tuple String Pattern) }
      | PWildcard
      | PConstructor { constructor :: QualifiedIdentifier, arguments :: Array Pattern } -- Replaces FunctionCall for patterns
      | PTuplePattern { elements :: Array Pattern }
      | PListPattern { elements :: Array Pattern }
      | PLiteral LiteralValue
      | PIdentifier String -- Simple identifier pattern
      | PParenthesized Pattern -- For ((pattern))

    data Type
      = TForAll { vars :: Array String, ttype :: Type }
      | TFunctionType { left :: Type, right :: Type } -- BinaryOp "->" becomes this
      | TRecordType { fields :: Array (Tuple String Type) }
      | TTypeApplication { constructor :: Type, arguments :: Array Type } -- Replaces FunctionCall for types
      | TTupleType { elements :: Array Type }
      | TListType Type -- For `[Int]` becomes TListType (TIdentifier "Int")
      | TIdentifier String -- For type names like "Int", "String" or type variables "a"
      | TQualifiedIdentifier (Array String) -- For type names like "Maybe.Maybe"
      | TParenthesized Type -- For ((Type))


    type AstIdentifier = String -- For now, qualified names are dot-separated strings

    data Module = Module
      { name :: AstIdentifier -- This would be a qualified name as a string
      }

    data ImportItem
      = ImportValue AstIdentifier
      | ImportType AstIdentifier (Maybe ImportSpecification)

    data ImportSpecification
      = ImportAllConstructors
      | ImportSpecificConstructors (Array AstIdentifier)

    data ImportDeclaration = ImportDeclaration
      { imodule :: AstIdentifier
      , alias :: Maybe AstIdentifier
      , items :: Maybe (Array ImportItem) -- Nothing means import all, Just [] means import ()
      , hiding :: Boolean
      }

    data TypeSignature = TypeSignature
      { name :: AstIdentifier
      , typeVars :: Array AstIdentifier -- Implicitly quantified
      , constraints :: Array Type -- Context
      , stype :: Type
      }

    data ForeignImport = ForeignImport
      { foreignModule :: Maybe String -- String literal for foreign module name
      , foreignFunction :: Maybe String -- String literal for foreign function name
      , alias :: AstIdentifier
      , typeSignature :: TypeSignature
      }

    data DataConstructor = DataConstructor
      { name :: AstIdentifier
      , fields :: Array Type
      }

    data DataType = DataType
      { name :: AstIdentifier
      , typeVars :: Array AstIdentifier
      , constructors :: Array DataConstructor
      }

    data TypeClass = TypeClass
      { name :: AstIdentifier
      , typeVars :: Array AstIdentifier
      , superclasses :: Array Type -- Placeholder for parsed superclass constraints
      , methods :: Array TypeSignature
      }

    data TypeClassInstance = TypeClassInstance
      { instanceName :: Maybe AstIdentifier -- For named instances
      , className :: AstIdentifier
      , ctype :: Type -- The type for which the instance is defined
      , constraints :: Array Type -- Instance context
      , methods :: Array FunctionDeclaration
      }

    data TypeAlias = TypeAlias
      { name :: AstIdentifier
      , typeVars :: Array AstIdentifier
      , aliasedType :: Type
      }

    data FunctionDeclaration = FunctionDeclaration
      { name :: AstIdentifier
      , parameters :: Array Pattern
      , body :: Expression
      , typeSignature :: Maybe TypeSignature
      }

    data CaseClause = CaseClause
      { pattern :: Pattern
      , guard :: Maybe Expression
      , body :: Expression
      }

    data Generator = Generator
      { pattern :: Pattern
      , expression :: Expression
      }

    data DoStatement
      = DoLet AstIdentifier Expression
      | DoBind Pattern Expression -- Pattern, not just identifier
      | DoExpr Expression


    data Decl
      = DModule Module
      | DImport ImportDeclaration
      | DForeignImport ForeignImport
      | DDataType DataType
      | DTypeAlias TypeAlias
      | DTypeClass TypeClass
      | DTypeClassInstance TypeClassInstance
      | DFunction FunctionDeclaration
      | DTypeSignature TypeSignature
    </types>

    <function>
    <old_code>#{func.code}</old_code>
    <name>#{func.name}</name>
    <description>#{func.description}</description>
    <local_deps>#{func.deps}</local_deps>
    </function>
    """
  end
end
