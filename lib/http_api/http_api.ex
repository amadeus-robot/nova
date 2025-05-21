defmodule MyAPI.Router do
  use Plug.Router

  # <--- CORS first so it can short-circuit OPTIONS
  plug(CORSPlug,
    origin: "*",
    # "GET,POST,PUT,PATCH,DELETE,OPTIONS" etc.
    methods: ["GET,POST,PUT,PATCH,DELETE,OPTIONS"],
    # allow any request header
    headers: ["*"]
  )

  plug(:match)
  plug(:dispatch)

  get("/ping", do: send_resp(conn, 200, "pong"))
  post("/echo", do: Plug.Conn.read_body(conn) |> elem(1) |> send_resp(conn, 200))

  get("/tasks.json",
    do:
      send_resp(
        conn,
        200,
        get_tasks()
      )
  )

  def get_tasks do
    fc = WorkFlow.Step0.go()

    tc =
      Enum.map(fc, fn x ->
        %{
          uid: x.name,
          title: x.name,
          content: x.description,
          children: desc_for_task(x),
          status: "In Progress"
        }
      end)

    r = [
      %{
        uid: "Parser",
        title: "Parser",
        content: "implementation of the parser",
        children: tc,
        status: "In Progress"
      }
    ]

    JSON.encode!(r)
  end

  def desc_for_task(x) do
    case File.read("wip/imps/#{x.name}") do
      {:ok, generated} ->
        xml = Prompts.ImplementationXmlParser.parse(generated)

        test =
          Enum.map(xml.tests, fn test ->
            res2 =
              Nova.Compiler.Tokenizer.tokenize(test)
              |> Nova.Compiler.Parser.parse_expression()
          end)

        compiles =
          try do
          catch
            _, _ -> :error
          end

        [
          %{
            uid: "gen:" <> x.name,
            title: "generated",
            content: "```purescript\n#{xml.code}\n```",
            children: [],
            status: "Done"
          }
        ] ++
          Enum.map(Enum.with_index(test), fn {t, idx} ->
            st =
              case t do
                {:ok, _, []} -> "In Progress"
                _ -> "Blocked"
              end

            %{
              uid: "test #{x.name} #{idx}",
              title: "test #{x.name} #{idx}",
              content: "```purescript\n\n```",
              children: [],
              status: st
            }
          end)

      _ ->
        []
    end
  end
end
