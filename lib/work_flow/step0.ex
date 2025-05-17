defmodule WorkFlow.Step0 do
  defmodule FunctionParser do
    @moduledoc """
    Parses function definitions from a specific XML-like format.
    """

    @function_regex ~r/<function>\s*<name>(.*?)<\/name>\s*<old_code>(.*?)<\/old_code>\s*<description>(.*?)<\/description>\s*<local_deps>(.*?)<\/local_deps>\s*<\/function>/s

    def parse_text(text) do
      Regex.scan(@function_regex, text)
      |> Enum.map(fn [_full_match, name_raw, code_raw, desc_raw, deps_raw] ->
        name = String.trim(name_raw)
        code = String.trim(code_raw)
        description = String.trim(desc_raw)

        deps =
          deps_raw
          |> String.trim()
          |> String.split(",", trim: true)
          |> Enum.map(&String.trim(&1))
          # Remove empty strings if deps is empty or has trailing commas
          |> Enum.reject(&(&1 == ""))

        %{
          name: name,
          code: code,
          description: description,
          deps: deps
        }
      end)
    end
  end

  defmodule FuncSorter do
    @type func :: %{
            name: String.t(),
            code: any,
            description: any,
            deps: [String.t()]
          }

    @spec sort([func]) :: [func]
    def sort(funcs) do
      by_name = Map.new(funcs, &{&1.name, &1})

      # keep only in-project deps; external ones don’t affect ordering
      graph =
        Enum.into(funcs, %{}, fn f ->
          {f.name, Enum.filter(f.deps, &Map.has_key?(by_name, &1))}
        end)

      {ordered_names, leftover_graph} = kahn(graph, [])
      ordered_funcs = Enum.map(ordered_names, &by_name[&1])
      cyclic_orphaned = leftover_graph |> Map.keys() |> Enum.sort() |> Enum.map(&by_name[&1])

      ordered_funcs ++ cyclic_orphaned
    end

    # ───────────────────────────────────────────────────────────
    # Kahn’s topo sort that just stops if it runs into a cycle
    # ───────────────────────────────────────────────────────────
    defp kahn(g, acc) do
      roots = for {n, []} <- g, do: n

      case roots do
        [] ->
          # either empty graph (done) or we’re in a cycle
          {Enum.reverse(acc), g}

        _ ->
          g1 =
            Enum.reduce(roots, g, fn r, g_acc ->
              g_acc
              |> Map.delete(r)
              |> Enum.map(fn {n, deps} -> {n, List.delete(deps, r)} end)
              |> Enum.into(%{})
            end)

          kahn(g1, roots ++ acc)
      end
    end
  end

  def go do
    fc = File.read!("wip/01_parser.meta")
    all = FunctionParser.parse_text(fc)
    FuncSorter.sort(all)
  end
end
