defmodule Nova.Compiler.TypedAst do
  @moduledoc """
  AST emitted *after* Hindley–Milner inference.

  Every expression and most declarations now carry `type :: Types.t()`,
  so later passes (code-gen, optimisation, linting) can rely on fully
  resolved types.
  """

  alias Nova.Compiler.Types, as: T

  # ─── Module & declarations ────────────────────────────────────────

  defmodule Module do
    defstruct [:name, :declarations]
  end

  defmodule FunctionDeclaration do
    @type t :: %__MODULE__{
            name: atom,
            parameters: [Identifier.t()],
            body: TypedAst.t(),
            type: T.t(),
            explicit_signature?: boolean
          }

    defstruct [:name, :parameters, :body, :type, explicit_signature?: false]
  end

  defmodule TypeSignature do
    # Left unchanged – it is metadata, not code.
    defstruct [:name, :type_vars, :constraints, :type]
  end

  defmodule ForeignImport do
    # The host (Elixir) gives us the type directly.
    defstruct [:module, :function, :alias, :type]
  end

  defmodule DataConstructor do
    defstruct [:name, :type, fields: []]
  end

  defmodule DataType do
    defstruct [:name, :type_vars, :constructors]
  end

  defmodule TypeAlias do
    defstruct [:name, :type_vars, :type]
  end

  defmodule UnaryOp do
    defstruct [:op, :value, :type]
  end

  defmodule Literal do
    defstruct [:value, :type]
  end

  defmodule Identifier do
    defstruct [:name, :type]
  end

  defmodule Lambda do
    defstruct [:parameters, :body, :type]
  end

  defmodule FunctionCall do
    defstruct [:function, :arguments, :type]
  end

  defmodule LetBinding do
    # bindings :: [{Identifier.t(), TypedAst.t()}]
    defstruct [:bindings, :body, :type]
  end

  defmodule BinaryOp do
    defstruct [:op, :left, :right, :type]
  end

  defmodule IfExpression do
    defstruct [:condition, :then_branch, :else_branch, :type]
  end

  defmodule CaseExpression do
    defstruct [:expression, :cases, :type]
  end

  defmodule CaseClause do
    # pattern remains raw for now
    defstruct [:pattern, :body]
  end

  # Containers
  defmodule List do
    defstruct [:elements, :type]
  end

  defmodule Tuple do
    defstruct [:elements, :type]
  end

  # Record literals / patterns added for step-1 row-type work
  defmodule RecordLiteral do
    # fields :: [{label :: String.t(), expr :: TypedAst.t()}]
    defstruct [:fields, :type]
  end

  # Convenience union
  @type t ::
          Literal.t()
          | Identifier.t()
          | Lambda.t()
          | FunctionCall.t()
          | LetBinding.t()
          | BinaryOp.t()
          | IfExpression.t()
          | CaseExpression.t()
          | List.t()
          | Tuple.t()
          | RecordLiteral.t()
end
