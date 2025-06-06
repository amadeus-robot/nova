defmodule WorkFlow.Step3 do
  def go() do
    {:ok, preamble_decls, []} = preamble()

    # we merge all the passing definitions into a single module
    # compile that module

    # we start with all the required funcs
    # we define a stub for all
    # the stub needs the right arity

    File.ls!("wip/imps")
    |> Enum.map(fn x ->
      p = Prompts.ImplementationXmlParser.parse(File.read!("wip/imps/#{x}"))

      try do
        res =
          Nova.Compiler.Tokenizer.tokenize(p.code) |> Nova.Compiler.Parser.parse_declarations()

        tests =
          Enum.map(p.tests, fn test ->
            res2 =
              Nova.Compiler.Tokenizer.tokenize(test)
              |> Nova.Compiler.Parser.parse_expression()

            case res2 do
              {:ok, code, []} ->
                try do
                  ns =
                    Nova.Compiler.CodeGen.compile_expression(
                      code,
                      namespace: "Nova.Compiler.V001.Parser",
                      generate_remote_calls: true
                    )

                  IO.puts(ns)

                  {:ok, ns}
                catch
                  _, _ ->
                    :error
                end

              _ ->
                IO.puts("--------------------------\r\n")
                IO.puts(test)
                :error
            end
          end)

        case res do
          {:ok, decls, []} ->
            ns =
              Nova.Compiler.CodeGen.compile(
                %Nova.Compiler.Ast.Module{
                  name: "Nova.Compiler.V001.Parser",
                  declarations: preamble_decls ++ decls
                },
                generate_remote_calls: true
              )

            try do
              Code.eval_string(ns)

              # IO.puts(ns)

              tests =
                Enum.map(tests, fn t ->
                  case t do
                    {:ok, c} ->
                      try do
                        {r, env} = Code.eval_string(c)
                        r
                      catch
                        a, b ->
                          {:crash, a, b}
                      end

                    x ->
                      x
                  end
                end)

              {:ok, x, tests}
            catch
              _, _ ->
                IO.inspect(ns)
                {:error, x, :code_eval_error}
            end

          _ ->
            {:error, x}
        end
      catch
        a, b -> {:crash, x, a, b, __STACKTRACE__}
      end
    end)
  end

  def preamble do
    source = """
    foreign import elixir "Elixir.Kernel" "inspect"          unsafeInspect        :: forall a. a -> String

    foreign import elixir "Elixir.Enum"   "drop_while"       elixirEnumDropWhile  :: forall a. (a -> Boolean) -> Array a -> Array a
    foreign import elixir "Elixir.Enum"   "reject"           elixirEnumReject     :: forall a. (a -> Boolean) -> Array a -> Array a
    foreign import elixir "Elixir.Enum"   "reverse"          elixirEnumReverse    :: forall a. Array a -> Array a
    foreign import elixir "Elixir.Enum"   "map"              elixirEnumMap        :: forall a b. (a -> b) -> Array a -> Array b
    foreign import elixir "Elixir.Enum"   "split_while"      elixirEnumSplitWhile :: forall a. (a -> Boolean) -> Array a -> Tuple (Array a) (Array a)
    foreign import elixir "Elixir.Enum"   "find_index"       elixirEnumFindIndex  :: forall a. (a -> Boolean) -> Array a -> Maybe Int
    foreign import elixir "Elixir.Enum"   "drop"             elixirEnumDrop       :: forall a. Int -> Array a -> Array a
    foreign import elixir "Elixir.Enum"   "join"             elixirEnumJoin       :: Array String -> String -> String

    foreign import elixir "Elixir.List"   "first"            elixirListFirst      :: forall a. Array a -> Maybe a

    foreign import elixir "Elixir.Kernel" "hd"               elixirHd             :: forall a. Array a -> Maybe a
    foreign import elixir "Elixir.Kernel" "length"           elixirLength         :: forall a. Array a -> Int
    """

    Nova.Compiler.Tokenizer.tokenize(source) |> Nova.Compiler.Parser.parse_declarations()
  end
end
