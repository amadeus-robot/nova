defmodule MyAPI.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # ─── Core domain processes (optional) ───────────────────────────
      {DynamicSupervisor, strategy: :one_for_one, name: MyAPI.DomainSupervisor},

      # ─── HTTP listener (Plug + Bandit) ─────────────────────────────
      {
        Bandit,
        # your Plug.Router or Plug.Builder module
        # bind to all IPv4 interfaces
        plug: MyAPI.Router, scheme: :http, ip: {0, 0, 0, 0}, port: 4000
      }
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
