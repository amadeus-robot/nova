defmodule HierarchicalFunctionManager do
  @moduledoc """
  Hierarchical Function Version Management with Per-Layer ETS Storage

  This system treats code as a collection of granular, individually-versioned
  functions organized in inheritance layers. Each layer has its own ETS table,
  and layers form a chain where each new layer becomes the head.

  ## Core Concepts:
  - Functions are the atomic unit of storage
  - Module declarations (use, require, import, alias) are stored per module
  - Each layer has its own ETS table
  - Layers form a parent-child chain with the newest layer as head
  - Changes are stored as deltas - only modified functions exist in child layers
  - Function resolution walks up the layer hierarchy until found
  - Modules can be rendered by collecting all functions and declarations for a module
  - Layers can be persisted to and loaded from disk

  ## Example Usage:
      iex> {:ok, manager} = HierarchicalFunctionManager.start_link()
      iex> {:ok, layer_1} = HierarchicalFunctionManager.create_layer(manager, 0)
      iex> HierarchicalFunctionManager.store_module_declarations(manager, "Math", [
      ...>   {:use, "GenServer"},
      ...>   {:require, "Logger"}
      ...> ])
      {:ok, ["Math.__declarations__"]}
      iex> HierarchicalFunctionManager.store_function(manager, "Math", "add", "def add(a, b), do: a + b", %{version: "1.0"})
      {:ok, ["Math.add"]}
      iex> HierarchicalFunctionManager.render_module(manager, "Math")
      {:ok, "defmodule Math do\n  use GenServer\n  require Logger\n\n  def add(a, b), do: a + b\nend"}
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
  Creates a new layer with the specified parent layer, becoming the new head.
  Returns {:ok, new_layer_id}.
  """
  def create_layer(pid \\ __MODULE__, parent_layer_id) do
    GenServer.call(pid, {:create_layer, parent_layer_id})
  end

  @doc """
  Stores module-level declarations (use, require, import, alias) for a module.
  declarations should be a list of tuples like:
  - {:use, "GenServer"}
  - {:require, "Logger"}
  - {:import, "Enum", only: [:map, :filter]}
  - {:alias, "MyApp.SomeModule", as: "SM"}
  Returns {:ok, [declaration_identifier]}.
  """
  def store_module_declarations(
        pid \\ __MODULE__,
        module_name,
        declarations,
        metadata \\ %{},
        layer_id \\ :head
      ) do
    GenServer.call(
      pid,
      {:store_module_declarations, layer_id, module_name, declarations, metadata}
    )
  end

  @doc """
  Retrieves module declarations from the specified layer, walking up the hierarchy if needed.
  Returns {:ok, {declarations, metadata, found_layer_id}} or {:error, :not_found}.
  """
  def get_module_declarations(pid \\ __MODULE__, module_name, layer_id \\ :head) do
    GenServer.call(pid, {:get_module_declarations, layer_id, module_name})
  end

  @doc """
  Stores a function in the head layer (default) or specified layer.
  Returns {:ok, [function_names]} where function_names is a list of stored function identifiers.
  """
  def store_function(
        pid \\ __MODULE__,
        module_name,
        function_name,
        code,
        metadata \\ %{},
        layer_id \\ :head
      ) do
    GenServer.call(pid, {:store_function, layer_id, module_name, function_name, code, metadata})
  end

  @doc """
  Stores multiple functions in the head layer (default) or specified layer.
  functions should be a list of {module_name, function_name, code, metadata} tuples.
  Returns {:ok, [function_names]} where function_names is a list of stored function identifiers.
  """
  def store_functions(pid \\ __MODULE__, functions, layer_id \\ :head) do
    GenServer.call(pid, {:store_functions, layer_id, functions})
  end

  @doc """
  Retrieves a function from the head layer (default) or specified layer, walking up the hierarchy if needed.
  Returns {:ok, {code, metadata, found_layer_id}} or {:error, :not_found}.
  """
  def get_function(pid \\ __MODULE__, module_name, function_name, layer_id \\ :head) do
    GenServer.call(pid, {:get_function, layer_id, module_name, function_name})
  end

  @doc """
  Renders a complete module by collecting all functions and declarations for the specified module name from head layer.
  Returns {:ok, module_code} or {:error, reason}.
  """
  def render_module(pid \\ __MODULE__, module_name, layer_id \\ :head) do
    GenServer.call(pid, {:render_module, layer_id, module_name})
  end

  @doc """
  Renders all modules visible from the head layer (default) or specified layer.
  Returns {:ok, %{module_name => module_code}} or {:error, reason}.
  """
  def render_all_modules(pid \\ __MODULE__, layer_id \\ :head) do
    GenServer.call(pid, {:render_all_modules, layer_id})
  end

  @doc """
  Lists all functions visible from the head layer (default) or specified layer.
  Returns a map of {module, function} => {code, metadata, layer_id}.
  """
  def list_functions(pid \\ __MODULE__, layer_id \\ :head) do
    GenServer.call(pid, {:list_functions, layer_id})
  end

  @doc """
  Lists all module declarations visible from the head layer (default) or specified layer.
  Returns a map of module_name => {declarations, metadata, layer_id}.
  """
  def list_module_declarations(pid \\ __MODULE__, layer_id \\ :head) do
    GenServer.call(pid, {:list_module_declarations, layer_id})
  end

  @doc """
  Gets the complete inheritance chain for a layer.
  Returns a list of layer IDs from the specified layer up to the root layer.
  """
  def get_layer_chain(pid \\ __MODULE__, layer_id \\ :head) do
    GenServer.call(pid, {:get_layer_chain, layer_id})
  end

  @doc """
  Gets the current head layer ID.
  """
  def get_head_layer(pid \\ __MODULE__) do
    GenServer.call(pid, :get_head_layer)
  end

  @doc """
  Lists all layer IDs in order from head to root.
  """
  def list_layers(pid \\ __MODULE__) do
    GenServer.call(pid, :list_layers)
  end

  @doc """
  Deletes a function from the head layer (default) or specified layer.
  This creates a "tombstone" that masks the function in parent layers.
  """
  def delete_function(pid \\ __MODULE__, module_name, function_name, layer_id \\ :head) do
    GenServer.call(pid, {:delete_function, layer_id, module_name, function_name})
  end

  @doc """
  Deletes module declarations from the head layer (default) or specified layer.
  This creates a "tombstone" that masks the declarations in parent layers.
  """
  def delete_module_declarations(pid \\ __MODULE__, module_name, layer_id \\ :head) do
    GenServer.call(pid, {:delete_module_declarations, layer_id, module_name})
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
  def compact_layer(pid \\ __MODULE__, layer_id \\ :head) do
    GenServer.call(pid, {:compact_layer, layer_id})
  end

  @doc """
  Saves all layers to disk using ETS tab2file.
  Returns :ok or {:error, reason}.
  """
  def save_to_disk(pid \\ __MODULE__, base_path \\ "./layers") do
    GenServer.call(pid, {:save_to_disk, base_path})
  end

  @doc """
  Loads all layers from disk using ETS file2tab.
  This will replace the current state with the loaded data.
  Returns :ok or {:error, reason}.
  """
  def load_from_disk(pid \\ __MODULE__, base_path \\ "./layers") do
    GenServer.call(pid, {:load_from_disk, base_path})
  end

  @doc """
  Lists all available layer files on disk.
  Returns {:ok, [filenames]} or {:error, reason}.
  """
  def list_disk_layers(base_path \\ "./layers") do
    case File.ls(base_path) do
      {:ok, files} ->
        layer_files =
          files
          |> Enum.filter(&String.ends_with?(&1, ".layers"))
          |> Enum.sort()

        {:ok, layer_files}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def clear(pid \\ __MODULE__) do
    GenServer.call(pid, :clear)
  end

  # Server Implementation

  defstruct [
    # Map of layer_id => ETS table reference
    :layer_tables,
    # ETS table for layer metadata
    :layers_table,
    # Map of layer_id => parent_layer_id
    :layer_hierarchy,
    # Current head layer ID
    :head_layer_id,
    # Next layer ID to assign
    :next_layer_id
  ]

  @impl true
  def init(_opts) do
    layers_table = :ets.new(:layers, [:set, :public])

    # Create root layer (layer 0)
    root_table = :ets.new(:layer_0, [:set, :public])

    :ets.insert(
      layers_table,
      {0,
       %{
         parent: nil,
         created_at: DateTime.utc_now(),
         function_count: 0,
         declaration_count: 0
       }}
    )

    state = %__MODULE__{
      layer_tables: %{0 => root_table},
      layers_table: layers_table,
      layer_hierarchy: %{0 => nil},
      head_layer_id: 0,
      next_layer_id: 1
    }

    Logger.info("HierarchicalFunctionManager started with root layer 0 as head")
    {:ok, state}
  end

  @impl true
  def handle_call(:clear, pid, state) do
    layers_table = :ets.new(:layers, [:set, :public])

    # Create root layer (layer 0)
    root_table = :ets.new(:layer_0, [:set, :public])

    :ets.insert(
      layers_table,
      {0,
       %{
         parent: nil,
         created_at: DateTime.utc_now(),
         function_count: 0,
         declaration_count: 0
       }}
    )

    state = %__MODULE__{
      layer_tables: %{0 => root_table},
      layers_table: layers_table,
      layer_hierarchy: %{0 => nil},
      head_layer_id: 0,
      next_layer_id: 1
    }

    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:create_layer, parent_layer_id}, _from, state) do
    cond do
      not Map.has_key?(state.layer_hierarchy, parent_layer_id) ->
        {:reply, {:error, :parent_not_found}, state}

      true ->
        new_layer_id = state.next_layer_id

        # Create new ETS table for this layer
        table_name = String.to_atom("layer_#{new_layer_id}")
        new_table = :ets.new(table_name, [:set, :public])

        # Insert layer metadata
        layer_meta = %{
          parent: parent_layer_id,
          created_at: DateTime.utc_now(),
          function_count: 0,
          declaration_count: 0
        }

        :ets.insert(state.layers_table, {new_layer_id, layer_meta})

        # Update state
        new_state = %{
          state
          | layer_tables: Map.put(state.layer_tables, new_layer_id, new_table),
            layer_hierarchy: Map.put(state.layer_hierarchy, new_layer_id, parent_layer_id),
            head_layer_id: new_layer_id,
            next_layer_id: state.next_layer_id + 1
        }

        Logger.info("Created layer #{new_layer_id} with parent #{parent_layer_id} (new head)")
        {:reply, {:ok, new_layer_id}, new_state}
    end
  end

  @impl true
  def handle_call(
        {:store_module_declarations, layer_id, module_name, declarations, metadata},
        _from,
        state
      ) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    case Map.get(state.layer_tables, actual_layer_id) do
      nil ->
        {:reply, {:error, :layer_not_found}, state}

      table ->
        declaration_id = "#{module_name}.__declarations__"
        key = {module_name, :__declarations__}
        value = {declarations, metadata, actual_layer_id}

        :ets.insert(table, {key, value})

        # Update layer declaration count
        case :ets.lookup(state.layers_table, actual_layer_id) do
          [{^actual_layer_id, layer_meta}] ->
            updated_meta = Map.update(layer_meta, :declaration_count, 1, &(&1 + 1))
            :ets.insert(state.layers_table, {actual_layer_id, updated_meta})

          [] ->
            :ok
        end

        Logger.debug("Stored declarations for module #{module_name} in layer #{actual_layer_id}")
        {:reply, {:ok, [declaration_id]}, state}
    end
  end

  @impl true
  def handle_call({:get_module_declarations, layer_id, module_name}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)
    key = {module_name, :__declarations__}
    result = resolve_function(state, actual_layer_id, key)
    {:reply, result, state}
  end

  @impl true
  def handle_call(
        {:store_function, layer_id, module_name, function_name, code, metadata},
        _from,
        state
      ) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    case Map.get(state.layer_tables, actual_layer_id) do
      nil ->
        {:reply, {:error, :layer_not_found}, state}

      table ->
        function_id = "#{module_name}.#{function_name}"
        key = {module_name, function_name}
        value = {code, metadata, actual_layer_id}

        :ets.insert(table, {key, value})

        # Update layer function count
        case :ets.lookup(state.layers_table, actual_layer_id) do
          [{^actual_layer_id, layer_meta}] ->
            updated_meta = Map.update(layer_meta, :function_count, 1, &(&1 + 1))
            :ets.insert(state.layers_table, {actual_layer_id, updated_meta})

          [] ->
            :ok
        end

        Logger.debug("Stored function #{function_id} in layer #{actual_layer_id}")
        {:reply, {:ok, [function_id]}, state}
    end
  end

  @impl true
  def handle_call({:store_functions, layer_id, functions}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    case Map.get(state.layer_tables, actual_layer_id) do
      nil ->
        {:reply, {:error, :layer_not_found}, state}

      table ->
        function_ids =
          Enum.map(functions, fn {module_name, function_name, code, metadata} ->
            function_id = "#{module_name}.#{function_name}"
            key = {module_name, function_name}
            value = {code, metadata, actual_layer_id}

            :ets.insert(table, {key, value})
            function_id
          end)

        # Update layer function count
        case :ets.lookup(state.layers_table, actual_layer_id) do
          [{^actual_layer_id, layer_meta}] ->
            updated_meta =
              Map.update(
                layer_meta,
                :function_count,
                length(functions),
                &(&1 + length(functions))
              )

            :ets.insert(state.layers_table, {actual_layer_id, updated_meta})

          [] ->
            :ok
        end

        Logger.debug("Stored #{length(functions)} functions in layer #{actual_layer_id}")
        {:reply, {:ok, function_ids}, state}
    end
  end

  @impl true
  def handle_call({:get_function, layer_id, module_name, function_name}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)
    key = {module_name, function_name}
    result = resolve_function(state, actual_layer_id, key)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:render_module, layer_id, module_name}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    if Map.has_key?(state.layer_hierarchy, actual_layer_id) do
      {declarations, functions} = collect_module_data(state, actual_layer_id, module_name)

      if Enum.empty?(declarations) and Enum.empty?(functions) do
        {:reply, {:error, :module_not_found}, state}
      else
        module_code = render_module_code(module_name, declarations, functions)
        {:reply, {:ok, module_code}, state}
      end
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:render_all_modules, layer_id}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    if Map.has_key?(state.layer_hierarchy, actual_layer_id) do
      all_data = collect_all_module_data(state, actual_layer_id)

      modules =
        all_data
        |> Enum.group_by(fn
          {{module_name, :__declarations__}, _data} -> module_name
          {{module_name, _function_name}, _data} -> module_name
        end)
        |> Enum.map(fn {module_name, module_items} ->
          {declarations, functions} = separate_declarations_and_functions(module_items)
          module_code = render_module_code(module_name, declarations, functions)
          {module_name, module_code}
        end)
        |> Enum.into(%{})

      {:reply, {:ok, modules}, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:list_functions, layer_id}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    if Map.has_key?(state.layer_hierarchy, actual_layer_id) do
      all_data = collect_all_module_data(state, actual_layer_id)

      functions =
        Enum.filter(all_data, fn
          {{_module_name, :__declarations__}, _data} -> false
          {{_module_name, _function_name}, _data} -> true
        end)

      {:reply, {:ok, functions}, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:list_module_declarations, layer_id}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    if Map.has_key?(state.layer_hierarchy, actual_layer_id) do
      all_data = collect_all_module_data(state, actual_layer_id)

      declarations =
        all_data
        |> Enum.filter(fn
          {{_module_name, :__declarations__}, _data} -> true
          {{_module_name, _function_name}, _data} -> false
        end)
        |> Enum.map(fn {{module_name, :__declarations__}, {declarations, metadata, layer}} ->
          {module_name, {declarations, metadata, layer}}
        end)
        |> Enum.into(%{})

      {:reply, {:ok, declarations}, state}
    else
      {:reply, {:error, :layer_not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_layer_chain, layer_id}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)
    chain = build_layer_chain(state.layer_hierarchy, actual_layer_id, [])
    {:reply, {:ok, chain}, state}
  end

  @impl true
  def handle_call(:get_head_layer, _from, state) do
    {:reply, {:ok, state.head_layer_id}, state}
  end

  @impl true
  def handle_call(:list_layers, _from, state) do
    chain = build_layer_chain(state.layer_hierarchy, state.head_layer_id, [])
    {:reply, {:ok, chain}, state}
  end

  @impl true
  def handle_call({:delete_function, layer_id, module_name, function_name}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    case Map.get(state.layer_tables, actual_layer_id) do
      nil ->
        {:reply, {:error, :layer_not_found}, state}

      table ->
        key = {module_name, function_name}
        # Store a tombstone marker
        tombstone = {:deleted, %{deleted_at: DateTime.utc_now()}, actual_layer_id}
        :ets.insert(table, {key, tombstone})

        Logger.debug(
          "Deleted function #{module_name}.#{function_name} in layer #{actual_layer_id}"
        )

        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call({:delete_module_declarations, layer_id, module_name}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    case Map.get(state.layer_tables, actual_layer_id) do
      nil ->
        {:reply, {:error, :layer_not_found}, state}

      table ->
        key = {module_name, :__declarations__}
        # Store a tombstone marker
        tombstone = {:deleted, %{deleted_at: DateTime.utc_now()}, actual_layer_id}
        :ets.insert(table, {key, tombstone})

        Logger.debug("Deleted declarations for module #{module_name} in layer #{actual_layer_id}")
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    total_entries =
      state.layer_tables
      |> Map.values()
      |> Enum.map(&:ets.info(&1, :size))
      |> Enum.sum()

    total_layers = map_size(state.layer_hierarchy)

    layer_stats =
      state.layer_hierarchy
      |> Enum.map(fn {layer_id, parent} ->
        [{_, layer_meta}] = :ets.lookup(state.layers_table, layer_id)
        {layer_id, Map.put(layer_meta, :parent, parent)}
      end)
      |> Enum.into(%{})

    stats = %{
      total_entries: total_entries,
      total_layers: total_layers,
      head_layer_id: state.head_layer_id,
      layers: layer_stats
    }

    {:reply, {:ok, stats}, state}
  end

  @impl true
  def handle_call({:compact_layer, layer_id}, _from, state) do
    actual_layer_id = resolve_layer_id(state, layer_id)

    if Map.has_key?(state.layer_hierarchy, actual_layer_id) and actual_layer_id != 0 do
      parent_layer_id = state.layer_hierarchy[actual_layer_id]
      compacted_count = compact_layer_functions(state, actual_layer_id, parent_layer_id)

      Logger.info(
        "Compacted layer #{actual_layer_id}, removed #{compacted_count} redundant entries"
      )

      {:reply, {:ok, compacted_count}, state}
    else
      {:reply, {:error, :invalid_layer}, state}
    end
  end

  @impl true
  def handle_call({:save_to_disk, base_path}, _from, state) do
    try do
      File.mkdir_p!(base_path)

      # Save each layer's ETS table
      Enum.each(state.layer_tables, fn {layer_id, table} ->
        layer_file = Path.join(base_path, "layer_#{layer_id}.layers")
        :ets.tab2file(table, String.to_charlist(layer_file))
      end)

      # Save layer metadata table
      layers_file = Path.join(base_path, "layers_metadata.layers")
      :ets.tab2file(state.layers_table, String.to_charlist(layers_file))

      # Save hierarchy and state as term files
      hierarchy_file = Path.join(base_path, "hierarchy.layers")
      state_file = Path.join(base_path, "state.layers")

      hierarchy_binary = :erlang.term_to_binary(state.layer_hierarchy)
      File.write!(hierarchy_file, hierarchy_binary)

      state_data = %{
        head_layer_id: state.head_layer_id,
        next_layer_id: state.next_layer_id,
        layer_ids: Map.keys(state.layer_tables)
      }

      state_binary = :erlang.term_to_binary(state_data)
      File.write!(state_file, state_binary)

      Logger.info("Saved layers to disk at #{base_path}")
      {:reply, :ok, state}
    rescue
      error ->
        Logger.error("Failed to save layers to disk: #{inspect(error)}")
        {:reply, {:error, error}, state}
    end
  end

  @impl true
  def handle_call({:load_from_disk, base_path}, _from, state) do
    try do
      hierarchy_file = Path.join(base_path, "hierarchy.layers")
      state_file = Path.join(base_path, "state.layers")
      layers_file = Path.join(base_path, "layers_metadata.layers")

      # Check if core files exist
      unless File.exists?(hierarchy_file) and File.exists?(state_file) and
               File.exists?(layers_file) do
        {:reply, {:error, :files_not_found}, state}
      else
        # Clear existing tables
        Enum.each(state.layer_tables, fn {_id, table} -> :ets.delete(table) end)
        :ets.delete_all_objects(state.layers_table)

        # Load hierarchy and state
        hierarchy_binary = File.read!(hierarchy_file)
        layer_hierarchy = :erlang.binary_to_term(hierarchy_binary)

        state_binary = File.read!(state_file)
        state_data = :erlang.binary_to_term(state_binary)

        # Load layers metadata table
        {:ok, _} = :ets.file2tab(String.to_charlist(layers_file))

        # Load each layer's ETS table
        layer_tables =
          Enum.reduce(state_data.layer_ids, %{}, fn layer_id, acc ->
            layer_file = Path.join(base_path, "layer_#{layer_id}.layers")

            if File.exists?(layer_file) do
              {:ok, table} = :ets.file2tab(String.to_charlist(layer_file))
              Map.put(acc, layer_id, table)
            else
              acc
            end
          end)

        new_state = %{
          state
          | layer_tables: layer_tables,
            layer_hierarchy: layer_hierarchy,
            head_layer_id: state_data.head_layer_id,
            next_layer_id: state_data.next_layer_id
        }

        Logger.info("Loaded layers from disk at #{base_path}")
        {:reply, :ok, new_state}
      end
    rescue
      error ->
        Logger.error("Failed to load layers from disk: #{inspect(error)}")
        {:reply, {:error, error}, state}
    end
  end

  # Private Helper Functions

  defp resolve_layer_id(state, :head), do: state.head_layer_id
  defp resolve_layer_id(_state, layer_id), do: layer_id

  defp resolve_function(state, layer_id, key) do
    chain = build_layer_chain(state.layer_hierarchy, layer_id, [])
    find_function_in_chain(state, chain, key)
  end

  defp find_function_in_chain(_state, [], _key) do
    {:error, :not_found}
  end

  defp find_function_in_chain(state, [layer_id | rest], key) do
    case Map.get(state.layer_tables, layer_id) do
      nil ->
        find_function_in_chain(state, rest, key)

      table ->
        case :ets.lookup(table, key) do
          [{^key, {:deleted, _meta, _layer}}] ->
            {:error, :not_found}

          [{^key, {code, metadata, found_layer}}] ->
            {:ok, {code, metadata, found_layer}}

          [] ->
            find_function_in_chain(state, rest, key)
        end
    end
  end

  defp build_layer_chain(_hierarchy, nil, acc), do: acc

  defp build_layer_chain(hierarchy, layer_id, acc) do
    parent = hierarchy[layer_id]
    build_layer_chain(hierarchy, parent, [layer_id | acc])
  end

  defp collect_all_module_data(state, layer_id) do
    chain = build_layer_chain(state.layer_hierarchy, layer_id, [])

    chain
    # Start from root layer
    |> Enum.reverse()
    |> Enum.reduce(%{}, fn current_layer, acc ->
      layer_data = get_layer_data(state, current_layer)
      Map.merge(acc, layer_data)
    end)
  end

  defp collect_module_data(state, layer_id, module_name) do
    all_data = collect_all_module_data(state, layer_id)

    {declarations, functions} =
      all_data
      |> Enum.filter(fn
        {{mod_name, :__declarations__}, _data} -> mod_name == module_name
        {{mod_name, _function_name}, _data} -> mod_name == module_name
      end)
      |> separate_declarations_and_functions()

    {declarations, functions}
  end

  defp separate_declarations_and_functions(items) do
    Enum.split_with(items, fn
      {{_module_name, :__declarations__}, _data} -> true
      {{_module_name, _function_name}, _data} -> false
    end)
    |> case do
      {[{{_module_name, :__declarations__}, {declarations, _metadata, _layer}}], functions} ->
        formatted_functions =
          functions
          |> Enum.map(fn {{_module_name, function_name}, {code, _metadata, _layer}} ->
            {function_name, code}
          end)
          |> Enum.sort_by(fn {function_name, _code} -> function_name end)

        {declarations, formatted_functions}

      {[], functions} ->
        formatted_functions =
          functions
          |> Enum.map(fn {{_module_name, function_name}, {code, _metadata, _layer}} ->
            {function_name, code}
          end)
          |> Enum.sort_by(fn {function_name, _code} -> function_name end)

        {[], formatted_functions}
    end
  end

  defp render_module_code(module_name, declarations, functions) do
    declaration_code =
      declarations
      |> Enum.map(&render_declaration/1)
      |> Enum.join("\n")

    function_code =
      functions
      |> Enum.map(fn {_function_name, code} -> "  #{code}" end)
      |> Enum.join("\n")

    parts =
      [
        "defmodule #{module_name} do",
        if(declaration_code != "", do: "  #{declaration_code}", else: nil),
        if(function_code != "", do: function_code, else: nil),
        "end"
      ]
      |> Enum.filter(&(&1 != nil))
      |> Enum.join(
        if declaration_code != "" and function_code != "" do
          "\n\n"
        else
          "\n"
        end
      )

    parts
  end

  defp render_declaration(a) when is_binary(a), do: a
  defp render_declaration({:use, module}), do: "use #{module}"
  defp render_declaration({:require, module}), do: "require #{module}"
  defp render_declaration({:import, module}), do: "import #{module}"

  defp render_declaration({:import, module, opts}) when is_list(opts) do
    opts_str =
      Enum.map_join(opts, ", ", fn
        {:only, funcs} when is_list(funcs) -> "only: #{inspect(funcs)}"
        {:except, funcs} when is_list(funcs) -> "except: #{inspect(funcs)}"
        {key, val} -> "#{key}: #{inspect(val)}"
      end)

    "import #{module}, #{opts_str}"
  end

  defp render_declaration({:alias, module}), do: "alias #{module}"

  defp render_declaration({:alias, module, opts}) when is_list(opts) do
    opts_str =
      Enum.map_join(opts, ", ", fn
        {:as, alias_name} -> "as: #{alias_name}"
        {key, val} -> "#{key}: #{inspect(val)}"
      end)

    "alias #{module}, #{opts_str}"
  end

  defp get_layer_data(state, layer_id) do
    case Map.get(state.layer_tables, layer_id) do
      nil ->
        %{}

      table ->
        :ets.tab2list(table)
        |> Enum.reduce(%{}, fn {key, value}, acc ->
          case value do
            {:deleted, _meta, _layer} ->
              Map.delete(acc, key)

            {code_or_declarations, metadata, found_layer} ->
              Map.put(acc, key, {code_or_declarations, metadata, found_layer})
          end
        end)
    end
  end

  defp compact_layer_functions(state, layer_id, parent_layer_id) do
    layer_data = get_layer_data(state, layer_id)

    {to_remove, _to_keep} =
      Enum.split_with(layer_data, fn {key, {code_or_declarations, metadata, _}} ->
        case resolve_function(state, parent_layer_id, key) do
          {:ok, {^code_or_declarations, ^metadata, _}} -> true
          _ -> false
        end
      end)

    # Remove redundant entries
    case Map.get(state.layer_tables, layer_id) do
      nil ->
        0

      table ->
        Enum.each(to_remove, fn {key, _} ->
          :ets.delete(table, key)
        end)

        length(to_remove)
    end
  end
end
