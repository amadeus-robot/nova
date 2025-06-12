defmodule LlmTaskProcessor.Manager do
  @moduledoc """
  The Manager GenServer.
  - Receives new task submissions.
  - Periodically checks for pending tasks and dispatches them to idle workers.
  - Manages worker processes.
  - Receives feedback from workers about task completion/failure.
  """
  use GenServer
  require Logger

  # Configure the number of workers and dispatch interval in config/config.exs
  @default_num_workers 3
  # Check for pending tasks every 1 second
  @dispatch_interval_ms 1000

  # --- Client API ---

  @doc """
  Starts the Manager GenServer.
  """
  @spec start_link() :: GenServer.on_start()
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Submits a new LLM task to the system.
  """
  @spec add_llm_task(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def add_llm_task(prompt) do
    GenServer.call(__MODULE__, {:add_task, prompt})
  end

  @doc """
  Retrieves the current status of a task.
  """
  @spec get_task_status(String.t()) :: LlmTaskProcessor.Task.t() | nil
  def get_task_status(task_id) do
    LlmTaskProcessor.TaskStore.get_task(task_id)
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(:ok) do
    num_workers = Application.get_env(:llm_task_processor, :num_workers, @default_num_workers)
    Logger.info("Manager starting with #{num_workers} workers.")

    # Start worker processes under a DynamicSupervisor
    # This allows for more flexible worker management (e.g., scaling up/down)
    {:ok, worker_sup} = DynamicSupervisor.start_link(name: LlmTaskProcessor.WorkerSupervisor)

    # Spawn initial workers
    worker_pids =
      1..num_workers
      |> Enum.map(fn id ->
        case DynamicSupervisor.start_child(worker_sup, {LlmTaskProcessor.Worker, id}) do
          {:ok, pid} ->
            Logger.info("Worker #{id} spawned with PID #{inspect(pid)}")
            pid

          {:error, reason} ->
            Logger.error("Failed to spawn worker #{id}: #{inspect(reason)}")
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    # Set up a periodic timer to dispatch tasks
    timer_ref = Process.send_after(self(), :dispatch_pending_tasks, @dispatch_interval_ms)

    {:ok, %{worker_sup: worker_sup, worker_pids: worker_pids, timer_ref: timer_ref}}
  end

  @impl true
  def handle_call({:add_task, prompt}, _from, state) do
    task = LlmTaskProcessor.Task.new(prompt)
    LlmTaskProcessor.TaskStore.add_task(task)
    Logger.info("New task submitted: #{task.id}")

    # Immediately try to dispatch tasks after a new one is added
    do_dispatch_tasks(state)

    {:reply, {:ok, task.id}, state}
  end

  @impl true
  def handle_info(:dispatch_pending_tasks, state) do
    # This message is sent by the periodic timer.
    do_dispatch_tasks(state)

    # Reset the timer for the next dispatch cycle
    new_timer_ref = Process.send_after(self(), :dispatch_pending_tasks, @dispatch_interval_ms)
    {:noreply, %{state | timer_ref: new_timer_ref}}
  end

  @impl true
  def handle_cast({:task_completed, task_id, worker_pid}, state) do
    Logger.info("Task #{task_id} completed by worker #{inspect(worker_pid)}.")
    # Attempt to dispatch another task immediately, as a worker is now free.
    do_dispatch_tasks(state)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:task_failed, task_id, reason, worker_pid}, state) do
    Logger.error("Task #{task_id} failed by worker #{inspect(worker_pid)}: #{reason}.")
    # Attempt to dispatch another task immediately, as a worker is now free.
    do_dispatch_tasks(state)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %{timer_ref: timer_ref}) do
    # Cancel the periodic timer when the manager shuts down
    Process.cancel_timer(timer_ref)
    Logger.info("Manager terminated. Timer cancelled.")
    :ok
  end

  # --- Internal Logic ---

  defp do_dispatch_tasks(state = %{worker_pids: worker_pids}) do
    # Find all idle workers
    idle_workers =
      Enum.filter(worker_pids, fn pid ->
        # Use GenServer.call for synchronous check, with a timeout
        case GenServer.call(pid, :is_idle, 100) do
          true ->
            true

          false ->
            false

          # Worker might have crashed or not responded
          _ ->
            Logger.warning("Worker #{inspect(pid)} did not respond to :is_idle check.")
            false
        end
      end)

    if idle_workers == [] do
      Logger.debug("No idle workers available.")
      state
    else
      # Get the next pending task and atomically mark it in-progress
      case LlmTaskProcessor.TaskStore.get_next_pending_task_for_processing(hd(idle_workers)) do
        {:ok, task_to_process} ->
          # Assign the task to the first available worker
          # Take the first idle worker
          worker_pid = hd(idle_workers)
          LlmTaskProcessor.Worker.solve_task(worker_pid, task_to_process)
          Logger.info("Dispatched task #{task_to_process.id} to worker #{inspect(worker_pid)}.")

          # Recursively call to dispatch more tasks if more workers are idle
          # This is a simple recursive call
          do_dispatch_tasks(state)

        :no_pending_tasks ->
          Logger.debug("No pending tasks to dispatch.")
          # No tasks to dispatch
          state
      end
    end
  end
end
