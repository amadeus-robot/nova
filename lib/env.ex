defmodule Nova.Compiler.Env do
  @moduledoc """
  Immutable typing environment.

  * `bindings` – identifier → `Scheme` (see `Nova.Compiler.Types.Scheme`)
  * `counter`  – monotonically-increasing integer used to generate fresh
                 type-variable IDs.
  """

  alias Nova.Compiler.Types.Scheme

  @type t :: %__MODULE__{
          bindings: %{optional(atom) => Scheme.t()},
          counter: non_neg_integer()
        }

  defstruct bindings: %{}, counter: 0

  @doc """
  An entirely empty environment (no bindings, counter = 0).
  """
  @spec empty() :: t
  def empty, do: %__MODULE__{}

  # ───────────────────────────────
  # A couple of helpers you’ll need
  # ───────────────────────────────

  @doc "Look up an identifier, returning `{:ok, scheme}` or `:error`."
  @spec lookup(t, atom) :: {:ok, Scheme.t()} | :error
  def lookup(%__MODULE__{bindings: bs}, name) do
    case bs do
      %{^name => scheme} -> {:ok, scheme}
      _ -> :error
    end
  end

  @doc "Extend the environment with a new identifier → scheme binding."
  @spec extend(t, atom, Scheme.t()) :: t
  def extend(%__MODULE__{} = env, name, scheme) do
    %{env | bindings: Map.put(env.bindings, name, scheme)}
  end

  @doc """
  Produce a fresh type-variable ID and bump the counter.
  Returns `{id, updated_env}`.
  """
  @spec fresh_var(t) :: {integer, t}
  def fresh_var(%__MODULE__{counter: n} = env), do: {n, %{env | counter: n + 1}}
end
