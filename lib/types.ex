defmodule Nova.Compiler.Types do
  @moduledoc """
  Core type representations used by the Hindley‑Milner type‑checker.
  """

  # A type variable is identified by an integer id (for fast comparisons)
  defmodule TVar do
    defstruct [:id, :name]
    def new(id, name), do: %__MODULE__{id: id, name: name}
  end

  # A concrete type constructor with zero or more parameters
  defmodule TCon do
    defstruct [:name, :args]
    def new(name, args \\ []), do: %__MODULE__{name: name, args: args}
  end

  defmodule Record do
    @moduledoc false
    # fields :: %{label => Types.t()}
    defstruct [:fields, :row]
  end

  @type t ::
          TVar.t()
          | TCon.t()
          # <── NEW
          | Record.t()

  # Convenience helpers for the common built‑ins
  def t_int, do: TCon.new(:Int)
  def t_string, do: TCon.new(:String)
  def t_char, do: TCon.new(:Char)
  def t_bool, do: TCon.new(:Bool)
  def t_list(el), do: TCon.new(:List, [el])
  def t_tuple(list), do: TCon.new({:Tuple, length(list)}, list)
  def t_arrow(a, b), do: TCon.new(:Fun, [a, b])

  # ---------------------------------------------------------------------------
  # Substitutions (Map from TVar.id => Types.t())
  # ---------------------------------------------------------------------------
  defmodule Subst do
    @type t :: %{optional(integer()) => Nova.Compiler.Types.t()}
    def new, do: %{}

    def lookup(sub, %TVar{id: id} = v), do: Map.get(sub, id, v)
    def lookup(_sub, t), do: t

    def single(%TVar{id: id}, t), do: %{id => t}

    def compose(s1, s2),
      do: Map.merge(Enum.into(s2, %{}, fn {k, v} -> {k, s_apply(s1, v)} end), s1)

    def s_apply(sub, %TVar{} = v), do: lookup(sub, v)

    def s_apply(sub, %TCon{args: args} = c) do
      %TCon{c | args: Enum.map(args, &s_apply(sub, &1))}
    end

    def s_apply(sub, %Record{fields: f} = r) do
      fm = for {k, v} <- f, into: %{}, do: {k, s_apply(sub, v)}
      %Record{r | fields: fm}
    end
  end

  # ---------------------------------------------------------------------------
  # Type Schemes (∀ quantified vars . type)
  # ---------------------------------------------------------------------------
  defmodule Scheme do
    defstruct [:vars, :type]
    def new(vars, type), do: %__MODULE__{vars: vars, type: type}
  end

  # ---------------------------------------------------------------------------
  # Environment mapping identifiers -> Scheme
  # ---------------------------------------------------------------------------

  defmodule Env do
    @moduledoc """
    Typing environment threaded through parsing → name‑resolution → HM inference.

    * `m`     – map of *type constructor name* ⇒ `Types.Scheme.t` (or any
                placeholder that at least carries the correct `vars` arity).
    * `counter` – monotonic integer used by `fresh_var/2`.
    * `registry_layer` – handle to the current `InterfaceRegistry` layer so
                         helper passes can fall back to upstream modules.
    """

    alias Nova.Compiler.Types
    alias Types.{Scheme, TVar, TCon}

    defstruct m: %{}, counter: 0, registry_layer: nil, namespace: nil

    # ────────────────────────────────────────────────────────────────────────────
    #  Constructors
    # ────────────────────────────────────────────────────────────────────────────

    @doc "Return a brand‑new empty environment (registry must be provided later)."
    @spec empty() :: t()
    def empty, do: %__MODULE__{m: builtin_prelude()}

    @doc "Return an empty env that already knows its InterfaceRegistry layer."
    @spec empty(layer) :: t() when layer: any
    def empty(layer), do: %__MODULE__{registry_layer: layer, m: builtin_prelude()}

    # ────────────────────────────────────────────────────────────────────────────
    #  Core ops
    # ────────────────────────────────────────────────────────────────────────────

    @type t :: %__MODULE__{}

    @doc "Add or replace a Scheme under `name`."
    @spec extend(t, atom, Scheme.t()) :: t
    def extend(%__MODULE__{m: m} = e, name, %Scheme{} = scheme) do
      %__MODULE__{e | m: Map.put(m, name, scheme)}
    end

    @doc "Lookup a Scheme by name.  Returns `{:ok, scheme}` | :error."
    @spec lookup(t, atom) :: {:ok, Scheme.t()} | :error
    def lookup(%__MODULE__{m: m}, name), do: Map.fetch(m, name)

    # ────────────────────────────────────────────────────────────────────────────
    #  Helpers used by NameResolver
    # ────────────────────────────────────────────────────────────────────────────

    @doc "Is the type constructor defined in the local environment?"
    @spec type_defined?(t, atom) :: boolean
    def type_defined?(env, name), do: match?({:ok, _}, lookup(env, name))

    @doc "Return the arity (number of type variables) of a locally defined type."
    @spec arity(t, atom) :: non_neg_integer | nil
    def arity(env, name) do
      case lookup(env, name) do
        {:ok, %Scheme{vars: vars}} -> length(vars)
        _ -> nil
      end
    end

    @doc "Bind a *new* data‑type during the walk so self‑recursive refs work."
    @spec bind_type(t, atom, non_neg_integer) :: t
    def bind_type(env, name, 0) do
      vars = []
      scheme = %Scheme{vars: vars, type: %TCon{name: name, args: vars}}
      extend(env, name, scheme)
    end

    def bind_type(env, name, arity) do
      vars = for i <- 0..(arity - 1), do: TVar.new(-1, String.to_atom("_#{i}"))
      scheme = %Scheme{vars: vars, type: %TCon{name: name, args: vars}}
      extend(env, name, scheme)
    end

    # ────────────────────────────────────────────────────────────────────────────
    #  Import handling (used by ImportLoader – future work)
    # ────────────────────────────────────────────────────────────────────────────

    @doc "Record a type imported under (possibly) an alias namespace."
    @spec bind_import(t, atom, atom, Scheme.t()) :: t
    def bind_import(env, _alias_ns, name, scheme) do
      # For now we ignore qualification; future versions may store {alias, name}.
      extend(env, name, scheme)
    end

    # ────────────────────────────────────────────────────────────────────────────
    #  Fresh TVar generator (still used by HM inference)
    # ────────────────────────────────────────────────────────────────────────────

    @doc "Generate a fresh type variable with an optional name hint."
    @spec fresh_var(t, String.t()) :: {TVar.t(), t}
    def fresh_var(%__MODULE__{counter: c} = e, hint \\ "t") do
      {TVar.new(c, String.to_atom(hint <> Integer.to_string(c))), %__MODULE__{e | counter: c + 1}}
    end

    # Build the Prelude map only once and reuse it – small but avoids re-allocs.
    @spec builtin_prelude() :: %{atom => Scheme.t()}
    defp builtin_prelude do
      # helpers ─────────────────────────────────────────────────────────────────
      fresh =
        Stream.iterate(-1, &(&1 - 1))
        |> Stream.map(&TVar.new(&1, :_))
        |> Enum.take(3)

      [v1, v2, v3] = fresh

      zero = []
      one = [v1]
      two = [v1, v2]

      %{
        # 0-arity
        Int: %Scheme{vars: zero, type: Types.t_int()},
        String: %Scheme{vars: zero, type: Types.t_string()},
        Char: %Scheme{vars: zero, type: Types.t_char()},
        Bool: %Scheme{vars: zero, type: Types.t_bool()},

        # 1-arity
        Array: %Scheme{vars: one, type: TCon.new(:Array, one)},
        List: %Scheme{vars: one, type: Types.t_list(v1)},
        Maybe: %Scheme{vars: one, type: TCon.new(:Maybe, one)},

        # 2-arity
        Either: %Scheme{vars: two, type: TCon.new(:Either, two)}
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Utility functions
  # ---------------------------------------------------------------------------
  def free_type_vars(%Record{fields: f}) do
    Enum.reduce(f, MapSet.new(), fn {_k, t}, acc ->
      MapSet.union(acc, free_type_vars(t))
    end)
  end

  def free_type_vars(%TVar{} = v), do: MapSet.new([v.id])

  def free_type_vars(%TCon{args: args}),
    do: Enum.reduce(args, MapSet.new(), fn t, acc -> MapSet.union(acc, free_type_vars(t)) end)

  def free_type_vars_scheme(%Scheme{vars: q, type: t}) do
    MapSet.difference(free_type_vars(t), MapSet.new(Enum.map(q, & &1.id)))
  end

  def free_type_vars_env(%Env{m: m}) do
    m
    |> Map.values()
    |> Enum.reduce(MapSet.new(), fn s, acc -> MapSet.union(acc, free_type_vars_scheme(s)) end)
  end

  # A type variable is identified by an integer id (for fast comparisons)
  defmodule TVar do
    defstruct [:id, :name]
    def new(id, name), do: %__MODULE__{id: id, name: name}
  end
end

defmodule Nova.Compiler.Unify do
  alias Nova.Compiler.Types.{TVar, TCon, Subst, Record}

  @spec unify(Types.t(), Types.t()) :: {:ok, Subst.t()} | {:error, String.t()}
  def unify(a, b), do: do_unify(a, b, Subst.new())

  defp do_unify(
         %Nova.Compiler.Types.Record{fields: f1},
         %Nova.Compiler.Types.Record{fields: f2},
         s
       ) do
    keys1 = Map.keys(f1) |> Enum.sort()
    keys2 = Map.keys(f2) |> Enum.sort()

    cond do
      keys1 != keys2 ->
        {:error, "Record label mismatch (#{inspect(keys1)} ≠ #{inspect(keys2)})"}

      true ->
        Enum.zip(keys1, keys1)
        |> Enum.reduce_while({:ok, s}, fn k, {:ok, sub} ->
          t1 = Types.Subst.s_apply(sub, Map.fetch!(f1, k))
          t2 = Types.Subst.s_apply(sub, Map.fetch!(f2, k))

          case do_unify(t1, t2, sub) do
            {:ok, sub2} -> {:cont, {:ok, sub2}}
            err -> {:halt, err}
          end
        end)
    end
  end

  defp do_unify(%TVar{id: i} = v, t, s) do
    case Subst.lookup(s, v) do
      ^v -> bind(i, t, s)
      t2 -> do_unify(t2, t, s)
    end
  end

  defp do_unify(t, %TVar{} = v, s), do: do_unify(v, t, s)

  defp do_unify(%TCon{name: n1, args: a1}, %TCon{name: n2, args: a2}, s)
       when n1 == n2 and length(a1) == length(a2) do
    Enum.zip(a1, a2)
    |> Enum.reduce_while({:ok, s}, fn {x, y}, {:ok, sub} ->
      case do_unify(Subst.s_apply(sub, x), Subst.s_apply(sub, y), sub) do
        {:ok, sub2} -> {:cont, {:ok, sub2}}
        err -> {:halt, err}
      end
    end)
  end

  defp do_unify(t1, t2, _s), do: {:error, "Cannot unify #{inspect(t1)} with #{inspect(t2)}"}

  defp occurs?(i, %TVar{id: j}), do: i == j
  defp occurs?(i, %TCon{args: args}), do: Enum.any?(args, &occurs?(i, &1))

  defp bind(i, t, _s) when is_struct(t, TVar) and t.id == i, do: {:ok, Subst.new()}

  defp bind(i, t, _s) do
    cond do
      occurs?(i, t) ->
        {:error, "Occurs check failed"}

      true ->
        {:ok, %{i => t}}
    end
  end
end
