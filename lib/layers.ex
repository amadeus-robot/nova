defmodule HierarchicalFunctionManager do
  @moduledoc """
  Hierarchical Function Version Management with ETS Storage

  This system treats code as a collection of granular, individually-versioned
  functions organized in inheritance layers. Each function is stored as a discrete
  unit in ETS tables, keyed by its fully-qualified identifier.

  ## Core Concepts:
  - Functions are the atomic unit of storage
  - Each layer inherits from its parent, creating a dependency chain
  - Changes are stored as deltas - only modified functions exist in child layers
  - Function resolution walks up the layer hierarchy until found

  ## Example Usage:
      iex> {:ok, manager} = HierarchicalFunctionManager.start_link()
      iex> HierarchicalFunctionManager.create_layer(manager, 1, 0)
      iex> HierarchicalFunctionManager.store_function(manager, 1, "Math", "add", "def add(a, b), do: a + b", %{version: "1.0"})
      iex> HierarchicalFunctionManager.get_function(manager, 1, "Math", "add")
      {:ok, {"def add(a, b), do: a + b", %{version: "1.0"}, 1}}
  """

  use GenServer
  require Logger

  # Client API

  @doc """
  Starts the hierarchical function manager.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Creates a new layer with the specified parent layer.
  Layer 0 is the base layer and has no parent.
  """
  def create_layer(pid \\ __MODULE__, layer_id, parent_layer_id \\ nil) do
    GenServer.call(pid, {:create_layer, layer_id, parent_layer_id})
  end

  @doc """
  Stores a function in the specified layer.
  """
  def store_function(pid \\ __MODULE__, layer_id, module_name, function_name, code, metadata \\ %{}) do
    GenServer.call(pid, {:store_function, layer_id, module_name, function_name, code, metadata})
  end

  @doc """
  Retrieves a function from the specified layer, walking up the hierarchy if needed.
  Returns {:ok, {code, metadata, found_layer_id}} or {:error, :not_found}.
  """
  def get_function(pid \\ __MODULE__, layer_id, module_name, function_name) do
    GenServer.call(pid, {:get_function, layer_id, module_name, function_name})
  end

  @doc """
  Lists all functions visible from the specified layer.
  Returns a map of {module, function} => {code, metadata, layer_id}.
  """
  def list_functions(pid \\ __MODULE__, layer_id) do
    GenServer.call(pid, {:list_functions, layer_id})
  end

  @doc """
  Gets the complete inheritance chain for a layer.
  Returns a list of layer IDs from the specified layer up to the base layer.
  """
  def get_layer_chain(pid \\ __MODULE__, layer_id) do
    GenServer.call(pid, {:get_layer_chain, layer_id})
  end

  @doc """
  Deletes a function from the specified layer.
  This creates a "tombstone" that masks the function in parent layers.
  """
  def delete_function(pid \\ __MODULE__, layer_id, module_name, function_name) do
    GenServer.call(pid, {:delete_function, layer_id, module_name, function_name})
  end

  @doc """
  Gets statistics about the function storage system.
  """
  def get_stats(pid \\ __MODULE__) do
    GenServer.call(pid, :get_stats)
  end

  @doc """
  Compacts a layer by removing functions that are identical to their parent layer.
  """
  def compact_layer(pid \\ __MODULE__, layer_id) do
    GenServer.call(pid, {:compact_layer, layer_id})
  end

  # Server Implementation

  defstruct [
    :functions_table,    # ETS table for function storage
    :layers_table,       # ETS table for layer metadata
    :layer_hierarchy     # Map of layer_id => parent_layer_id
  ]

  @impl true
  def init(_opts) do
    functions_table = :ets.new(:functions, [:set, :protected, :named_table])
    layers_table = :ets.new(:layers, [:set, :protected, :named_table])
    
    # Create base layer (layer 0)
    :ets.insert(layers_table, {0, %{parent: nil, created_at: DateTime.utc_now()}})
    
    state = %__MODULE__{
      functions_table: functions_table,
      layers_table: layers_table,
      layer_hierarchy: %{0 => nil}
    }
    
    Logger.info("HierarchicalFunctionManager started with base layer 0")
    {:ok, state}
  end

  @impl true
  def handle_call({:create_layer, layer_id, parent_layer_id}, _from, state) do
    cond do
      Map.has_key?(state.layer_hierarchy, layer_id) ->
        {:reply, {:error, :layer_exists}, state}
      
      parent_layer_id != nil and not Map.has_key?(state.layer_hierarchy, parent_layer_id) ->
        {:reply, {:error, :parent_not_found}, state}
      
      true ->
        # Insert layer metadata
        layer_meta = %{
          parent: parent_layer_id,
          created_at: DateTime.utc_now(),
          function_count: 0
        }
        :ets.insert(state.layers_table, {layer_id, layer_meta})
        
        # Update hierarchy
        new_hierarchy = Map.put(state.layer_hierarchy, layer_id, parent_layer_id)
        new_state = %{state | layer_hierarchy: new_hierarchy}
        
        Logger.info("Created layer #{layer_id} with parent #{parent_layer_id}")
        {:reply, :ok, new_state}
    end
  end

  @impl true
  def handle_call({:store_function, layer_id, module_name, function_name, code, metadata}, _from, state) do
    if Map.has_key?(state.layer_hierarchy, layer_id) do
      key = {module_name, function_name}
      value = {code, metadata, layer_id}
      
      # Store in functions table with layer-specific key
      storage_key = {layer_id, key}
      :ets.insert(state.functions_table, {storage_key, value})
      
      # Update layer function count
      case :ets.lookup(state.layers_table, layer_id) do
        [{^layer_id, layer_meta}] ->
          updated_meta = Map.update(layer_meta, :function_count, 1, &(&1 + 1))
          :ets.insert(state.layers_table, {layer_id, updated_meta})
        [] ->
          :ok
      end
      
      Logger.debug("Stored function #{module_name}.#{function_name} in layer #{layer_id}")
      {:reply, :ok, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_function, layer_id, module_name, function_name}, _from, state) do
    key = {module_name, function_name}
    result = resolve_function(state, layer_id, key)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:list_functions, layer_id}, _from, state) do
    if Map.has_key?(state.layer_hierarchy, layer_id) do
      functions = collect_all_functions(state, layer_id)
      {:reply, {:ok, functions}, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_layer_chain, layer_id}, _from, state) do
    chain = build_layer_chain(state.layer_hierarchy, layer_id, [])
    {:reply, {:ok, chain}, state}
  end

  @impl true
  def handle_call({:delete_function, layer_id, module_name, function_name}, _from, state) do
    if Map.has_key?(state.layer_hierarchy, layer_id) do
      key = {module_name, function_name}
      storage_key = {layer_id, key}
      
      # Store a tombstone marker
      tombstone = {:deleted, %{deleted_at: DateTime.utc_now()}, layer_id}
      :ets.insert(state.functions_table, {storage_key, tombstone})
      
      Logger.debug("Deleted function #{module_name}.#{function_name} in layer #{layer_id}")
      {:reply, :ok, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    total_functions = :ets.info(state.functions_table, :size)
    total_layers = map_size(state.layer_hierarchy)
    
    layer_stats = 
      state.layer_hierarchy
      |> Enum.map(fn {layer_id, parent} ->
        [{_, layer_meta}] = :ets.lookup(state.layers_table, layer_id)
        {layer_id, Map.put(layer_meta, :parent, parent)}
      end)
      |> Enum.into(%{})
    
    stats = %{
      total_functions: total_functions,
      total_layers: total_layers,
      layers: layer_stats
    }
    
    {:reply, {:ok, stats}, state}
  end

  @impl true
  def handle_call({:compact_layer, layer_id}, _from, state) do
    if Map.has_key?(state.layer_hierarchy, layer_id) and layer_id != 0 do
      parent_layer_id = state.layer_hierarchy[layer_id]
      compacted_count = compact_layer_functions(state, layer_id, parent_layer_id)
      
      Logger.info("Compacted layer #{layer_id}, removed #{compacted_count} redundant functions")
      {:reply, {:ok, compacted_count}, state}
    else
      {:reply, {:error, :invalid_layer}, state}
    end
  end

  # Private Helper Functions

  defp resolve_function(state, layer_id, key) do
    chain = build_layer_chain(state.layer_hierarchy, layer_id, [])
    find_function_in_chain(state, chain, key)
  end

  defp find_function_in_chain(_state, [], _key) do
    {:error, :not_found}
  end

  defp find_function_in_chain(state, [layer_id | rest], key) do
    storage_key = {layer_id, key}
    
    case :ets.lookup(state.functions_table, storage_key) do
      [{^storage_key, {:deleted, _meta, _layer}}] ->
        {:error, :not_found}
      
      [{^storage_key, {code, metadata, found_layer}}] ->
        {:ok, {code, metadata, found_layer}}
      
      [] ->
        find_function_in_chain(state, rest, key)
    end
  end

  defp build_layer_chain(_hierarchy, nil, acc), do: acc
  defp build_layer_chain(hierarchy, layer_id, acc) do
    parent = hierarchy[layer_id]
    build_layer_chain(hierarchy, parent, [layer_id | acc])
  end

  defp collect_all_functions(state, layer_id) do
    chain = build_layer_chain(state.layer_hierarchy, layer_id, [])
    
    chain
    |> Enum.reverse()  # Start from base layer
    |> Enum.reduce(%{}, fn current_layer, acc ->
      layer_functions = get_layer_functions(state, current_layer)
      Map.merge(acc, layer_functions)
    end)
  end

  defp get_layer_functions(state, layer_id) do
    pattern = {{layer_id, :'$1'}, :'$2'}
    
    :ets.match(state.functions_table, pattern)
    |> Enum.reduce(%{}, fn [key, value], acc ->
      case value do
        {:deleted, _meta, _layer} ->
          Map.delete(acc, key)
        
        {code, metadata, found_layer} ->
          Map.put(acc, key, {code, metadata, found_layer})
      end
    end)
  end

  defp compact_layer_functions(state, layer_id, parent_layer_id) do
    layer_functions = get_layer_functions(state, layer_id)
    
    {to_remove, _to_keep} = 
      Enum.split_with(layer_functions, fn {key, {code, metadata, _}} ->
        case resolve_function(state, parent_layer_id, key) do
          {:ok, {^code, ^metadata, _}} -> true
          _ -> false
        end
      end)
    
    # Remove redundant functions
    Enum.each(to_remove, fn {key, _} ->
      storage_key = {layer_id, key}
      :ets.delete(state.functions_table, storage_key)
    end)
    
    length(to_remove)
  end
end
