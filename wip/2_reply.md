module Nova.Compiler.ParserSignatures where

import Nova.Compiler.Ast
import Nova.Compiler.Lexer (Token) -- Assuming Token type is from Lexer
import Data.Tuple (Tuple)
import Data.Maybe (Maybe)
import Data.Array (Array)

-- The ParseOutput type, assumed from the AST definitions
-- data ParseOutput a = Ok a (Array Token) | Error String

-- Utility Token Manipulators

skip_newlines :: Array Token -> Array Token
drop_newlines :: Array Token -> Array Token
strip_newlines :: Array Token -> Array Token
ensure_consumed :: Array Token -> ParseOutput Unit -- Unit for ':ok'
split_type_and_rest :: String -> Array Token -> Tuple (Array Token) (Array Token)
skip_superclass_constraints :: Array Token -> Tuple (Array Token) (Array Token) -- Returns (remaining_tokens, constraint_tokens)
drop_instance_constraints :: Array Token -> Array Token
split_until_newline :: Array Token -> Tuple (Array Token) (Array Token)
skip_until_end :: Array Token -> Array Token

-- Layout and Conditional Helpers

is_pattern_start? :: Array Token -> Boolean
clause_start? :: Array Token -> Boolean
take_body :: Int -> Array Token -> Tuple (Array Token) (Array Token) -- Int is initial indent, acc is internal

-- Expect Functions (Consume specific tokens)

expect_colon :: Array Token -> ParseOutput String -- Returns ":" on success
expect_keyword :: String -> Array Token -> ParseOutput String -- Returns expected keyword string
expect_operator :: String -> Array Token -> ParseOutput String -- Returns expected operator string
expect_delimiter :: String -> Array Token -> ParseOutput String -- Returns expected delimiter string

-- Core Parser Combinators (Generic Helpers)

parse_any :: forall a. Array (Array Token -> ParseOutput a) -> Array Token -> ParseOutput a
parse_many :: forall a. (Array Token -> ParseOutput a) -> Array Token -> ParseOutput (Array a)
parse_separated :: forall a sep. (Array Token -> ParseOutput a) -> (Array Token -> ParseOutput sep) -> Array Token -> ParseOutput (Array a)

-- Literal and Identifier Parsers

parse_literal :: Array Token -> ParseOutput Expression -- Returns ELiteral
parse_string_literal :: Array Token -> ParseOutput String -- Returns the raw string value
parse_label :: Array Token -> ParseOutput String -- Returns label name as String
parse_identifier :: Array Token -> ParseOutput String -- Returns identifier name as String
parse_qualified_identifier :: Array Token -> ParseOutput String -- Returns qualified name as dot-separated String

-- Pattern Parsers

parse_record_pattern :: Array Token -> ParseOutput Pattern
parse_record_field_pattern :: Array Token -> ParseOutput (Tuple String Pattern) -- (label_name, pattern_ast)
parse_simple_pattern :: Array Token -> ParseOutput Pattern
parse_pattern :: Array Token -> ParseOutput Pattern
parse_constructor_pattern :: Array Token -> ParseOutput Pattern
parse_cons_pattern :: Array Token -> ParseOutput Pattern
parse_wildcard_pattern :: Array Token -> ParseOutput Pattern
parse_tuple_pattern :: Array Token -> ParseOutput Pattern
parse_list_pattern :: Array Token -> ParseOutput Pattern

-- Type Parsers

parse_type :: Array Token -> ParseOutput Type
parse_forall_type :: Array Token -> ParseOutput Type
parse_function_type :: Array Token -> ParseOutput Type
parse_record_type :: Array Token -> ParseOutput Type
parse_record_field :: Array Token -> ParseOutput (Tuple String Type) -- (label_name, type_ast)
parse_type_term :: Array Token -> ParseOutput Type
parse_list_type :: Array Token -> ParseOutput Type
parse_tuple_type :: Array Token -> ParseOutput Type
parse_basic_type :: Array Token -> ParseOutput Type
parse_type_atom :: Array Token -> ParseOutput Type

-- Expression Parsers (General)

parse_expression :: Array Token -> ParseOutput Expression
parse_binary_expression :: Array Token -> ParseOutput Expression -- Entry for precedence parsing
parse_dollar_expression :: Array Token -> ParseOutput Expression
parse_logical_expression :: Array Token -> ParseOutput Expression
parse_comparison_expression :: Array Token -> ParseOutput Expression
parse_additive_expression :: Array Token -> ParseOutput Expression
parse_multiplicative_expression :: Array Token -> ParseOutput Expression
parse_application :: Array Token -> ParseOutput Expression
collect_application_args :: Int -> Array Expression -> Array Token -> Tuple (Array Expression) (Array Token) -- Helper for parse_application
parse_term :: Array Token -> ParseOutput Expression

-- Specific Expression Form Parsers

parse_lambda :: Array Token -> ParseOutput Expression
parse_let_expression :: Array Token -> ParseOutput Expression
parse_binding :: Array Token -> ParseOutput (Tuple Pattern Expression) -- For let bindings
parse_if_expression :: Array Token -> ParseOutput Expression
parse_case_expression :: Array Token -> ParseOutput Expression
parse_case_clauses :: Expression -> Array Token -> ParseOutput Expression -- Takes scrutinee, returns full ECaseExpression
parse_case_clause :: Array Token -> ParseOutput CaseClause
maybe_parse_guard :: Array Token -> ParseOutput (Maybe Expression)
parse_do_block :: Array Token -> ParseOutput Expression
parse_do_expression :: Array Token -> ParseOutput DoStatement
parse_record_literal :: Array Token -> ParseOutput Expression
parse_record_field_expr :: Array Token -> ParseOutput (Tuple String Expression) -- (label_name, value_expr)
parse_list_literal :: Array Token -> ParseOutput Expression
parse_list_comprehension :: Array Token -> ParseOutput Expression
parse_generator :: Array Token -> ParseOutput Generator
parse_tuple_literal :: Array Token -> ParseOutput Expression

-- Declaration Parsers

parse_declarations :: Array Token -> ParseOutput (Array Decl)
parse_declaration :: Array Token -> ParseOutput Decl
parse_module :: Array Token -> ParseOutput Module
parse_import :: Array Token -> ParseOutput ImportDeclaration
parse_import_alias :: Array Token -> Tuple (Maybe AstIdentifier) (Array Token) -- Returns (Maybe AliasString, RemainingTokens)
parse_import_selectors :: Array Token -> ParseOutput { items :: Array ImportItem, hiding :: Boolean }
parse_paren_import_list :: Array Token -> ParseOutput (Array ImportItem)
parse_import_item :: Array Token -> ParseOutput ImportItem
parse_constructors :: AstIdentifier -> Array Token -> ParseOutput ImportSpecification -- Takes type name, parses constructor list
parse_foreign_import_simple :: Array Token -> ParseOutput ForeignImport
parse_foreign_import :: Array Token -> ParseOutput ForeignImport
parse_data_declaration :: Array Token -> ParseOutput DataType
parse_data_constructors :: Array Token -> ParseOutput (Array DataConstructor)
parse_data_constructor :: Array Token -> ParseOutput DataConstructor
parse_type_class :: Array Token -> ParseOutput TypeClass
parse_type_class_instance :: Array Token -> ParseOutput TypeClassInstance
parse_function_with_type_signature :: Array Token -> ParseOutput FunctionDeclaration
parse_type_signature :: Array Token -> ParseOutput TypeSignature
parse_type_alias :: Array Token -> ParseOutput TypeAlias
parse_function_declaration :: Array Token -> ParseOutput FunctionDeclaration
parse_function_parameters :: Array Token -> ParseOutput (Array Pattern)
