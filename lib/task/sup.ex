defmodule LlmTaskProcessor.WorkerSupervisor do
  @moduledoc """
  A DynamicSupervisor for LlmTaskProcessor.Worker processes.
  Allows workers to be started and stopped dynamically by the Manager.
  """
  use DynamicSupervisor

  @impl true
  def init(_args) do
    DynamicSupervisor.init(
      # The strategy :one_for_one means if one worker crashes, only that worker is restarted.
      # Other strategies like :rest_for_one or :one_for_all could be used depending on desired crash behavior.
      strategy: :one_for_one
    )
  end
end
