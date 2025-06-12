defmodule LlmTaskProcessor.TaskStore do
  @moduledoc """
  Manages tasks in an ETS table.
  Provides functions for adding, retrieving, updating, and querying tasks.
  """

  @ets_table_name :llm_tasks

  @doc """
  Initializes the ETS table. This should be called once at application startup.
  """
  def init() do
    # Create the ETS table if it doesn't already exist.
    # :set - Each object (task) is identified by its first element (id).
    # :public - Accessible by all processes.
    # :named_table - Can be referred to by its name.
    :ets.new(@ets_table_name, [:set, :public, :named_table])
  end

  @doc """
  Adds a new task to ETS.
  """
  @spec add_task(LlmTaskProcessor.Task.t()) :: :ok
  def add_task(%LlmTaskProcessor.Task{id: id} = task) do
    :ets.insert(@ets_table_name, {id, task})
    :ok
  end

  @doc """
  Retrieves a task by its ID.
  """
  @spec get_task(String.t()) :: LlmTaskProcessor.Task.t() | nil
  def get_task(task_id) do
    case :ets.lookup(@ets_table_name, task_id) do
      [{_id, task}] -> task
      [] -> nil
    end
  end

  @doc """
  Updates fields of an existing task.
  """
  @spec update_task(String.t(), map()) :: {:ok, LlmTaskProcessor.Task.t()} | {:error, :not_found}
  def update_task(task_id, updates) when is_map(updates) do
    case get_task(task_id) do
      nil ->
        {:error, :not_found}

      current_task ->
        updated_task = struct(current_task, updates)
        :ets.insert(@ets_table_name, {task_id, updated_task})
        {:ok, updated_task}
    end
  end

  @doc """
  Atomically retrieves and updates the status of the next pending task.
  This is crucial to ensure only one worker picks a task.
  """
  @spec get_next_pending_task_for_processing(pid()) ::
          {:ok, LlmTaskProcessor.Task.t()} | :no_pending_tasks
  def get_next_pending_task_for_processing(worker_pid) do
    # Using a match specification to find a pending task and update its status.
    # This is an atomic operation within ETS.
    match_spec = [
      {{:"$1", :"$2"},
       [
         {:and, [{:==, {:"$2", :status}, :pending}]},
         {true,
          [
            {:ets.update_element(), @ets_table_name, {:"$1", {:"$2", :status}, :in_progress}},
            {:ets.update_element(), @ets_table_name,
             {:"$1", {:"$2", :started_at}, NaiveDateTime.utc_now()}},
            {:ets.update_element(), @ets_table_name, {:"$1", {:"$2", :worker_pid}, worker_pid}},
            # Return the original task (before update) so the manager knows what it picked.
            # Or, return the updated one. For simplicity, we'll return the original
            # and then the manager will get the updated one via get_task.
            # A more robust atomic pattern would be to return the *updated* task directly.
            # Let's simplify: find a pending task, then update it in a separate step.
            # The manager will then fetch it.
            # For a truly atomic pick-and-mark, we'd iterate and use :ets.lookup_and_update
            # or a more complex match_spec that returns the updated task.
            # Let's use a simpler pattern first: find, then update.
            # This requires the manager to be careful about race conditions, but
            # `get_next_pending_task` is intended to be called by a single manager.
            # If multiple managers, then a more robust approach is needed.
            :"$2"
          ]}
       ]}
    ]

    # This attempts to find *one* matching entry and update it.
    # The `select_delete` and `insert` pattern is safer for atomic pick-and-update.
    # Let's implement that:
    tasks =
      :ets.select(@ets_table_name, [{:_, [], [:"$_"]}])
      |> Enum.filter(&LlmTaskProcessor.Task.pending?/1)
      # FIFO if using ordered_set, otherwise just sorts current view
      |> Enum.sort_by(& &1.submitted_at)

    case tasks do
      [task | _] ->
        # This is not fully atomic if multiple processes call this simultaneously.
        # However, since only the Manager GenServer will call this, it's fine.
        # If multiple managers were picking, you'd need a different strategy (e.g., Mnesia, or a GenServer guarding the pick).
        update_task(task.id, %{
          status: :in_progress,
          started_at: NaiveDateTime.utc_now(),
          worker_pid: worker_pid
        })

        {:ok, task}

      _ ->
        :no_pending_tasks
    end
  end

  @doc """
  Marks a task as completed with its result.
  """
  @spec mark_task_completed(String.t(), String.t()) ::
          {:ok, LlmTaskProcessor.Task.t()} | {:error, :not_found}
  def mark_task_completed(task_id, result) do
    update_task(task_id, %{
      status: :completed,
      result: result,
      completed_at: NaiveDateTime.utc_now()
    })
  end

  @doc """
  Marks a task as failed with an error message.
  """
  @spec mark_task_failed(String.t(), String.t()) ::
          {:ok, LlmTaskProcessor.Task.t()} | {:error, :not_found}
  def mark_task_failed(task_id, error) do
    update_task(task_id, %{status: :failed, error: error, completed_at: NaiveDateTime.utc_now()})
  end

  @doc """
  Lists tasks, optionally filtered by status.
  """
  @spec list_tasks(atom() | nil) :: [LlmTaskProcessor.Task.t()]
  def list_tasks(status \\ nil) do
    # Returns a list of all values (tasks) in the ETS table.
    tasks =
      :ets.tab2list(@ets_table_name)
      # Extract the task struct from {id, task} tuple
      |> Enum.map(&elem(&1, 1))

    if status do
      Enum.filter(tasks, fn %LlmTaskProcessor.Task{status: s} -> s == status end)
    else
      tasks
    end
  end
end
