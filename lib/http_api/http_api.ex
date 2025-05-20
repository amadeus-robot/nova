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
        ]

      _ ->
        []
    end
  end
end
