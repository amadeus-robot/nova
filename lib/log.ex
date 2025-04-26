defmodule Nova.Log do
  @moduledoc """
  Git‑style, append‑only log of Nova declarations with
  fast selective invalidation / revalidation.

  Four ETS tables back the implementation (all named, public):

    * `:nova_log_entries`      – `id => %Entry{}`
    * `:nova_log_name_index`   – `name => head‑id`
    * `:nova_log_reverse`      – BAG of `{dep_id, dependant_id}`
    * `:nova_log_memo`         – `id => compiled_artifact`

  The GenServer only serialises *writes*; all reads go directly to
  ETS for lock‑free speed.
  """

  use GenServer
  require Logger

  # ─────────────────────────────────────────────────────────────
  #  Public API
  # ─────────────────────────────────────────────────────────────

  @typedoc """
  Opaque handle for a log entry
  """
  @type id :: pos_integer()

  @doc "Start and link the log server. Include it in your supervision tree."
  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, :ok, opts)

  @doc "Return the current head entry‑id for a fully‑qualified `name`, or nil."
  @spec head_id(String.t()) :: id() | nil
  def head_id(name) do
    case(:ets.lookup(:nova_log_name_index, name)) do
      [{^name, id}] -> id
      [] -> nil
    end
  end

  @doc "Fetch a log entry struct by id (or nil)."
  @spec entry(id()) :: Entry.t() | nil
  def entry(id) do
    case(:ets.lookup(:nova_log_entries, id)) do
      [{^id, entry}] -> entry
      [] -> nil
    end
  end

  @doc "Return dependants (direct only) for an id."
  @spec dependants(id()) :: [id()]
  def dependants(id), do: :ets.lookup(:nova_log_reverse, id) |> Enum.map(fn {^id, dep} -> dep end)

  @doc "Append a new declaration. `kind` is `:type` or `:function`.
  `name` is fully‑qualified. `source` is the raw Nova/Elixir slice.
  `deps` must already be resolved to log‑ids (direct only).
  Returns `{:ok, id}` or `{:error, reason}`."
  @spec push(:type | :function, String.t(), String.t(), [id()]) :: {:ok, id()} | {:error, any()}
  def push(kind, name, source, deps \\ []) do
    GenServer.call(__MODULE__, {:push, kind, name, source, deps})
  end

  @doc "Invalidate a given entry id and everything that depends on it, recursively.
  Returns the set of invalidated ids (MapSet)."
  @spec invalidate(id()) :: MapSet.t(id())
  def invalidate(id), do: GenServer.call(__MODULE__, {:invalidate, id})

  # ─────────────────────────────────────────────────────────────
  #  Entry struct
  # ─────────────────────────────────────────────────────────────

  defmodule Entry do
    @enforce_keys [:id, :kind, :name, :payload, :deps, :timestamp]
    defstruct [:id, :kind, :name, :payload, :deps, :timestamp, :author]

    @type t :: %__MODULE__{
            id: pos_integer(),
            kind: :type | :function,
            name: String.t(),
            payload: binary(),
            deps: [pos_integer()],
            timestamp: DateTime.t(),
            author: term() | nil
          }
  end

  # ─────────────────────────────────────────────────────────────
  #  GenServer callbacks
  # ─────────────────────────────────────────────────────────────

  @impl true
  def init(:ok) do
    new_table = fn name, opts -> :ets.new(name, [:named_table, :public] ++ opts) end

    _entries = new_table.(:nova_log_entries, [:set])
    _name_index = new_table.(:nova_log_name_index, [:set])
    _reverse_graph = new_table.(:nova_log_reverse, [:bag, {:read_concurrency, true}])
    _memo_cache = new_table.(:nova_log_memo, [:set])

    {:ok, %{}}
  end

  @impl true
  def handle_call({:push, kind, name, source, deps}, _from, state) do
    id = :erlang.unique_integer([:monotonic, :positive])
    ts = DateTime.utc_now()
    entry = %Entry{id: id, kind: kind, name: name, payload: source, deps: deps, timestamp: ts}

    # 1. Compile BEFORE mutating tables so we can roll back on failure
    compile_result = safe_compile(source)

    with {:ok, artifact} <- compile_result do
      # 2. Commit to ETS (single process -> atomic per table)
      :ets.insert(:nova_log_entries, {id, entry})
      :ets.insert(:nova_log_name_index, {name, id})
      Enum.each(deps, fn dep -> :ets.insert(:nova_log_reverse, {dep, id}) end)
      :ets.insert(:nova_log_memo, {id, artifact})

      {:reply, {:ok, id}, state}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:invalidate, start_id}, _from, state) do
    invalidated = do_invalidate(start_id)
    {:reply, invalidated, state}
  end

  # ─────────────────────────────────────────────────────────────
  #  Helpers
  # ─────────────────────────────────────────────────────────────

  # Compile Nova‑generated Elixir source into BEAM and return module list.
  defp safe_compile(source) do
    try do
      {:ok, Code.compile_string(source)}
    rescue
      err -> {:error, err}
    end
  end

  # Breadth‑first walk over dependants, dropping memo cache and purge modules.
  defp do_invalidate(id) do
    queue = :queue.from_list([id])
    walk(queue, MapSet.new())
  end

  defp walk(queue, seen) do
    case :queue.out(queue) do
      {{:value, cur}, rest} ->
        if MapSet.member?(seen, cur) do
          walk(rest, seen)
        else
          purge(cur)
          next = dependants(cur)
          walk(Enum.reduce(next, rest, &:queue.in(&2, &1)), MapSet.put(seen, cur))
        end

      {:empty, _} ->
        # TODO: revalidate in topo order – left as future work
        seen
    end
  end

  defp purge(id) do
    case :ets.take(:nova_log_memo, id) do
      [{^id, modules}] ->
        Enum.each(modules, fn {m, _bin} ->
          :code.purge(m)
          :code.delete(m)
        end)

      [] ->
        :ok
    end
  end
end
