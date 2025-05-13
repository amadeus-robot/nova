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

  @type t :: TVar.t() | TCon.t()

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
    defstruct [:m, :counter]
    def empty, do: %__MODULE__{m: %{}, counter: 0}

    def extend(%__MODULE__{m: m} = e, name, scheme),
      do: %__MODULE__{e | m: Map.put(m, name, scheme)}

    def lookup(%__MODULE__{m: m}, name), do: Map.fetch(m, name)

    def fresh_var(%__MODULE__{counter: c} = e, hint \\ "t") do
      {TVar.new(c, String.to_atom(hint <> Integer.to_string(c))), %__MODULE__{e | counter: c + 1}}
    end
  end

  # ---------------------------------------------------------------------------
  # Utility functions
  # ---------------------------------------------------------------------------
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
  alias Nova.Compiler.Types.{TVar, TCon, Subst}

  @spec unify(Types.t(), Types.t()) :: {:ok, Subst.t()} | {:error, String.t()}
  def unify(a, b), do: do_unify(a, b, Subst.new())

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
