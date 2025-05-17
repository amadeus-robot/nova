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
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:beaver, "~> 0.4"},
      {:llvm_config, "~> 0.1"},
      {:sweet_xml, "~> 0.7"}
      {:rocksdb, git: "https://github.com/xenomorphtech/erlang-rocksdb.git", override: true},
      {:mnesia_kv, git: "https://github.com/xenomorphtech/mnesia_kv.git"}
    ]
  end
end
