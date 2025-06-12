defmodule LlmTaskProcessor.Application do
  @moduledoc """
  Supervisor for the LlmTaskProcessor application.
  Initializes the ETS table and starts the Manager GenServer.
  """
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Initialize the ETS table (not a GenServer, but its init/0 needs to run)
      # This ensures the table exists before anyone tries to use it.
      # We'll use a wrapper child spec for this.
      {LlmTaskProcessor.TaskStore, :init, []},

      # Start the Task Manager GenServer
      # It will manage the workers and dispatch tasks.
      LlmTaskProcessor.Manager
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    Supervisor.start_link(children, strategy: :one_for_one, name: LlmTaskProcessor.Supervisor)
  end
end

defmodule LlmTaskProcessor.Task do
  @moduledoc """
  Defines the struct for an LLM processing task.
  """

  # Define the fields for a task.
  # Using UUIDs for IDs for uniqueness and easy generation.
  # NaiveDateTime for timestamps, as timezone isn't critical for internal processing.
  defstruct id: nil,
            prompt: nil,
            # :pending, :in_progress, :completed, :failed, :cancelled
            status: :pending,
            result: nil,
            error: nil,
            submitted_at: nil,
            started_at: nil,
            completed_at: nil,
            # PID of the worker currently processing this task
            worker_pid: nil

  @doc """
  Creates a new pending task.
  """
  def new(prompt) do
    %__MODULE__{
      # Requires `:ecto_sql` dependency or manual UUID generation
      id: Ecto.UUID.generate(),
      prompt: prompt,
      submitted_at: NaiveDateTime.utc_now()
    }
  end

  # --- Helper functions for status checks (optional but good practice) ---

  @doc "Checks if the task is pending."
  def pending?(%__MODULE__{status: :pending}), do: true
  def pending?(_), do: false

  @doc "Checks if the task is in progress."
  def in_progress?(%__MODULE__{status: :in_progress}), do: true
  def in_progress?(_), do: false

  @doc "Checks if the task is completed."
  def completed?(%__MODULE__{status: :completed}), do: true
  def completed?(_), do: false

  @doc "Checks if the task has failed."
  def failed?(%__MODULE__{status: :failed}), do: true
  def failed?(_), do: false

  @doc "Checks if the task was cancelled."
  def cancelled?(%__MODULE__{status: :cancelled}), do: true
  def cancelled?(_), do: false
end
