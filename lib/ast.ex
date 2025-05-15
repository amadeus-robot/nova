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

  # Represent each import item as

  # String.t() – plain identifier (log)

  # {mod, :all} – data-constructor wildcard (Foo(..))

  # {mod, [ctor1, ctor2]} – explicit ctor list (Foo(Bar,Baz)).

  defmodule ImportDeclaration do
    defstruct module: nil,
              # "P" in `import Prelude as P`
              alias: nil,
              # ["log"] or [{"Foo", :all}] …
              items: [],
              # true when the keyword 'hiding' is present
              hiding?: false
  end

  defmodule Wildcard do
    # no payload, it matches anything
    defstruct []
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
    defstruct [:pattern, :guard, :body]
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

  defmodule ForAllType do
    # vars :: [String.t()],  type :: Ast.t()
    defstruct [:vars, :type]
  end

  defmodule RecordType do
    # row :: :empty | {:var, String.t()} | :wild
    defstruct [:fields, :row]
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
