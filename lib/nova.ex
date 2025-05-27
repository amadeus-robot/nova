defmodule Nova.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # ─── Core domain processes (optional) ───────────────────────────
      {DynamicSupervisor, strategy: :one_for_one, name: MyAPI.DomainSupervisor},

      # {MCPServer.TcpServer, 5000},
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
