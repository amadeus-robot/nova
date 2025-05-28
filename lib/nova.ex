defmodule Nova.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    # if starting on the playground, gotta not start mcp services

    children = [
      # ─── Core domain processes (optional) ───────────────────────────
      {DynamicSupervisor, strategy: :one_for_one, name: MyAPI.DomainSupervisor},
      {MCPServer, [port: 5000]},
      {HierarchicalFunctionManager, []}
    ]

    children =
      if Mix.env() == :test do
        []
      else
        children
      end

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: MyAPI.Supervisor
    )
  end
end
