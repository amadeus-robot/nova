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
    JSON.encode!([
      %{
        uid: "task-1-1-1",
        title: "Schema Design",
        content: "Design new database schema",
        children: [],
        status: "Done"
      }
    ])
  end
end
