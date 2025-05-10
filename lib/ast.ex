defmodule Nova.Compiler.Ast do
  # Module definition
  defmodule Module do
    defstruct [:name, :declarations]
  end

  # Declarations
  defmodule TypeDeclaration do
    defstruct [:name, :type_signature]
  end

  defmodule FunctionDeclaration do
    defstruct [:name, :parameters, :body, :type_signature]
  end

  defmodule TypeSignature do
    defstruct [:name, :type_vars, :constraints, :type]
  end

  defmodule TypeClass do
    defstruct [:name, :type_vars, :methods]
  end

  defmodule TypeAlias do
    defstruct [:name, :type_vars, :type]
  end

  defmodule TypeClassInstance do
    defstruct [:class_name, :type, :methods]
  end

  defmodule DataType do
    defstruct [:name, :type_vars, :constructors]
  end

  defmodule DataConstructor do
    defstruct [:name, :fields]
  end

  defmodule ImportDeclaration do
    defstruct [:module, :imports]
  end

  defmodule ForeignImport do
    defstruct [:module, :function, :alias, :type_signature]
  end

  # Expressions
  defmodule FunctionCall do
    defstruct [:function, :arguments]
  end

  defmodule BinaryOp do
    defstruct [:op, :left, :right]
  end

  defmodule UnaryOp do
    defstruct [:op, :value]
  end

  defmodule IfExpression do
    defstruct [:condition, :then_branch, :else_branch]
  end

  defmodule CaseExpression do
    defstruct [:expression, :cases]
  end

  defmodule CaseClause do
    defstruct [:pattern, :body]
  end

  defmodule LetBinding do
    defstruct [:bindings, :body]
  end

  defmodule DoBlock do
    defstruct [:expressions]
  end

  defmodule Lambda do
    defstruct [:parameters, :body]
  end

  defmodule List do
    defstruct [:elements]
  end

  defmodule ListComprehension do
    defstruct [:expression, :generators, :guards]
  end

  defmodule Generator do
    defstruct [:pattern, :expression]
  end

  defmodule Tuple do
    defstruct [:elements]
  end

  defmodule Identifier do
    defstruct [:name]
  end

  defmodule Literal do
    defstruct [:type, :value]
  end

  defmodule PrefixOperator do
    defstruct [:op, :expression]
  end

  defmodule RecordType do
    # [{label :: String.t(), type :: t()}]
    defstruct [:fields]
  end

  defmodule RecordPattern do
    # [{label, pattern}, …]
    defstruct [:fields]
  end

  defmodule RecordLiteral do
    # [{label, expression}, …]
    defstruct [:fields]
  end
end
