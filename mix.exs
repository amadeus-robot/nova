defmodule Nova3.MixProject do
  use Mix.Project

  def project do
    [
      app: :nova3,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Nova.Application, :start}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:beaver, "~> 0.4"},
      {:llvm_config, "~> 0.1"},
      {:sweet_xml, "~> 0.7.5"},
      {:rocksdb, git: "https://github.com/xenomorphtech/erlang-rocksdb.git", override: true},
      {:mnesia_kv, git: "https://github.com/xenomorphtech/mnesia_kv.git"},
      {:plug, "~> 1.15"},
      {:bandit, "~> 1.4"},
      {:cors_plug, "~> 3.0"},
      {:finch, "~> 0.19"},
      {:jason, "~> 1.4"}
    ]
  end
end
