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
          children: [],
          status: "In Progress"
        }
      end)

    JSON.encode!(tc)
  end
end
