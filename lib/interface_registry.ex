defmodule Nova.Compiler.InterfaceRegistry do
  @moduledoc """
  Layer‑aware in‑memory store for typed module interfaces.

  * Each layer owns **two anonymous ETS tables** (both `:set`):
    * **Schemes table** – `{namespace, elem}` → `Types.Scheme.t()`
    * **AST table**     – `{namespace, elem}` → `TypedAst.t()`
  * A layer may point to a **parent** layer; look‑ups walk the chain
    (`current → parent → …`) until a hit is found.
  * Layers are isolated: writes always affect the *current* layer only.
  * When a layer process exits, its ETS tables disappear automatically.
  """

  alias Nova.Compiler.{TypedAst, Types}
  alias Types.Scheme

  defstruct schemes_tab: nil, ast_tab: nil, parent: nil

  @type namespace :: atom
  @type elem_name :: atom
  @type t :: %__MODULE__{
          schemes_tab: :ets.tid(),
          ast_tab: :ets.tid(),
          parent: t | nil
        }

  # ---------------------------------------------------------------------------
  # Layer lifecycle helpers
  # ---------------------------------------------------------------------------

  @doc """
  Create an *empty* root registry (no parent).  Use this once at VM start‑up.
  """
  @spec new_root!() :: t
  def new_root! do
    %__MODULE__{schemes_tab: new_tab(), ast_tab: new_tab(), parent: nil}
  end

  @doc """
  Begin a new *batch* layer that isolates further edits from `parent` until
  you either `commit_batch!/1` or `discard_batch/1`.
  """
  @spec begin_batch(t) :: t
  def begin_batch(parent) do
    %__MODULE__{schemes_tab: new_tab(), ast_tab: new_tab(), parent: parent}
  end

  @doc """
  Permanently merges `batch` into its parent (must not be root) and deletes
  the batch tables.  Returns the parent layer for convenience.
  """
  @spec commit_batch!(t) :: t | no_return
  def commit_batch!(%__MODULE__{parent: nil}),
    do: raise(ArgumentError, "cannot commit the root layer")

  def commit_batch!(%__MODULE__{schemes_tab: s, ast_tab: a, parent: p} = batch) do
    # Merge Schemes ----------------------------------------------------------
    :ets.foldl(fn {k, v}, :ok -> :ets.insert(p.schemes_tab, {k, v}) end, :ok, s)

    # Merge AST --------------------------------------------------------------
    :ets.foldl(fn {k, v}, :ok -> :ets.insert(p.ast_tab, {k, v}) end, :ok, a)

    discard_batch(batch)
    p
  end

  @doc """
  Deletes a layer without merging.  Useful for cancelling WIP edits.
  """
  @spec discard_batch(t) :: :ok
  def discard_batch(%__MODULE__{schemes_tab: s, ast_tab: a}) do
    :ets.delete(s)
    :ets.delete(a)
    :ok
  end

  # ---------------------------------------------------------------------------
  # Put helpers (write current layer only)
  # ---------------------------------------------------------------------------

  @spec put_scheme(t, namespace, elem_name, Scheme.t()) :: :ok
  def put_scheme(%__MODULE__{schemes_tab: s}, ns, name, %Scheme{} = sch) do
    :ets.insert(s, {{ns, name}, sch})
    :ok
  end

  @spec put_ast(t, namespace, elem_name, TypedAst.t()) :: :ok
  def put_ast(%__MODULE__{ast_tab: a}, ns, name, %{} = ast) do
    :ets.insert(a, {{ns, name}, ast})
    :ok
  end

  @doc "Convenience wrapper that stores scheme and AST in one go."
  @spec put(t, namespace, elem_name, Scheme.t(), TypedAst.t()) :: :ok
  def put(layer, ns, name, %Scheme{} = sch, %{} = ast) do
    put_scheme(layer, ns, name, sch)
    put_ast(layer, ns, name, ast)
  end

  # ---------------------------------------------------------------------------
  # Lookup helpers (follow parent chain)
  # ---------------------------------------------------------------------------

  @spec get_scheme(t, namespace, elem_name) :: {:ok, Scheme.t()} | :error
  def get_scheme(layer, ns, name) do
    lookup(:schemes_tab, layer, {ns, name})
  end

  @spec get_ast(t, namespace, elem_name) :: {:ok, TypedAst.t()} | :error
  def get_ast(layer, ns, name) do
    lookup(:ast_tab, layer, {ns, name})
  end

  @doc "List all element names exported by a namespace (top‑most definition wins)."
  @spec list_namespace(t, namespace) :: [elem_name]
  def list_namespace(%__MODULE__{} = layer, ns) do
    # Collect keys from all layers; head shadows tail.
    do_list_ns(layer, ns, %{}) |> Map.keys()
  end

  # ---------------------------------------------------------------------------
  # Internal utilities
  # ---------------------------------------------------------------------------

  defp new_tab do
    :ets.new(:nova_interface, [:set, :public, read_concurrency: true])
  end

  # recursive lookup through parent chain
  defp lookup(tab_key, %__MODULE__{} = layer, key) do
    case :ets.lookup(Map.fetch!(layer, tab_key), key) do
      [{^key, val}] ->
        {:ok, val}

      [] ->
        case layer.parent do
          nil -> :error
          parent -> lookup(tab_key, parent, key)
        end
    end
  end

  # gather all names for a namespace through layers
  defp do_list_ns(%__MODULE__{schemes_tab: s, parent: p}, ns, acc) do
    acc2 =
      :ets.match(s, {{ns, :"$1"}, :"$2"})
      |> Enum.reduce(acc, fn [name, _], map -> Map.put_new(map, name, true) end)

    case p do
      nil -> acc2
      parent -> do_list_ns(parent, ns, acc2)
    end
  end

  @doc """
  Return a map `%{ident => Types.Scheme.t()}` with *all* schemes that belong
  to `namespace` in the given `layer`.  No visibility rules for now – every
  published binding is considered public.
  """
  @spec list_exports(t(), atom) :: {:ok, %{atom => Nova.Compiler.Types.Scheme.t()}} | :error
  def list_exports(layer, ns) do
    case :ets.lookup(layer.schemes_tab, {ns, :__all__}) do
      [] ->
        # Fallback: fold over the whole table (slower, but works for now)
        exports =
          :ets.foldl(
            fn
              {{^ns, ident}, scheme}, acc -> Map.put(acc, ident, scheme)
              _, acc -> acc
            end,
            %{},
            layer.schemes_tab
          )

        # Return :error if the namespace is completely unknown
        if map_size(exports) == 0, do: :error, else: {:ok, exports}

      [{_, map}] ->
        # If you already store a pre-built exports map under {ns, :__all__}
        {:ok, map}
    end
  end
end
