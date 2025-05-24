defmodule JsonrpcServer.TcpServer do
  use GenServer
  require Logger

  def start_link(port) do
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  @impl true
  def init(port) do
    {:ok, listen_socket} =
      :gen_tcp.listen(port, [
        :binary,
        packet: :line,
        active: false,
        reuseaddr: true
      ])

    Logger.info("JSON-RPC TCP server listening on port #{port}")

    # Start accepting connections
    spawn_link(fn -> accept_loop(listen_socket) end)

    {:ok, %{listen_socket: listen_socket, port: port}}
  end

  defp accept_loop(listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, client_socket} ->
        Logger.info("New client connected")

        # Spawn a process to handle this client
        spawn(fn -> __MODULE__.handle_client(client_socket) end)

        # Continue accepting new connections
        accept_loop(listen_socket)

      {:error, reason} ->
        Logger.error("Failed to accept connection: #{inspect(reason)}")
        accept_loop(listen_socket)
    end
  end

  def handle_client(socket, rest \\ "") do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        IO.inspect({:got, data})

        json_data = rest <> data

        case Jason.decode(json_data) do
          {:ok, request} ->
            response = __MODULE__.handle_jsonrpc_request(request)
            response_json = Jason.encode!(response) <> "\n"

            :gen_tcp.send(socket, response_json)

            # Continue handling requests from this client
            __MODULE__.handle_client(socket)

          {:error, error} ->
            __MODULE__.handle_client(socket, json_data)
        end

      {:error, :closed} ->
        Logger.info("Client disconnected")
        :gen_tcp.close(socket)

      {:error, reason} ->
        Logger.error("Error receiving data: #{inspect(reason)}")
        :gen_tcp.close(socket)
    end
  end

  def handle_jsonrpc_request(%{"jsonrpc" => "2.0", "method" => method, "id" => id} = request) do
    params = Map.get(request, "params", [])

    IO.inspect({:request, method, id, params})

    res =
      try do
        dispatch_method(method, params)
      catch
        a, b ->
          {:error, -32600, inspect({a, b})}
      end

    case res do
      {:ok, result} ->
        %{
          "jsonrpc" => "2.0",
          "result" => result,
          "id" => id
        }

      {:error, code, message} ->
        create_error_response(id, code, message)
    end
  end

  def handle_jsonrpc_request(%{"jsonrpc" => "2.0", "method" => method} = request) do
    # Notification (no id field) - don't send response
    IO.inspect({:notification, method, request["id"]})
    params = Map.get(request, "params", [])
    dispatch_method(method, params)
    nil
  end

  def handle_jsonrpc_request(%{"id" => id}) do
    create_error_response(id, -32600, "Invalid Request")
  end

  def handle_jsonrpc_request(_) do
    create_error_response(nil, -32600, "Invalid Request")
  end

  # Protocol version validation
  def validate_protocol_version("2024-11-05"), do: :ok
  # Also support older version
  def validate_protocol_version("2024-10-07"), do: :ok

  def validate_protocol_version(version) do
    {:error,
     "Unsupported protocol version: #{version}. Supported versions: 2024-11-05, 2024-10-07"}
  end

  def dispatch_method("initialize", params) do
    client_info = Map.get(params, "clientInfo", %{})
    protocol_version = Map.get(params, "protocolVersion", "2024-11-05")
    capabilities = Map.get(params, "capabilities", %{})

    Logger.info("MCP Initialize request from client: #{inspect(client_info)}")
    Logger.info("Protocol version: #{protocol_version}")
    Logger.info("Client capabilities: #{inspect(capabilities)}")

    # Validate protocol version
    case validate_protocol_version(protocol_version) do
      :ok ->
        result = %{
          "protocolVersion" => "2024-11-05",
          "capabilities" => %{"tools" => %{}},
          "serverInfo" => %{
            "name" => "elixir-mcp-server",
            "version" => "1.0.0"
          },
          "instructions" => "use this server to do elixir tasks"
        }

        {:ok, result}

      {:error, message} ->
        {:error, -32602, message}
    end
  end

  def dispatch_method("prompts/list", params) do
    {:ok, %{"prompts" => []}}
  end

  def dispatch_method("resources/list", params) do
    {:ok, %{"prompts" => []}}
  end

  def dispatch_method("tools/list", params) do
    tools = %{
      "tools" => [
        %{
          "name" => "eval_elixir_snippet",
          "description" => "runs an snippet of elixir code and returns the results",
          "inputSchema" => %{
            "properties" => %{"code" => %{"type" => "string"}},
            "required" => ["code"],
            "type" => "object"
          }
        },
        %{
          "name" => "hfm_create_layer",
          "description" =>
            "Creates a new layer with the specified parent layer, becoming the new head layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "parent_layer_id" => %{
                "type" => "integer",
                "description" => "ID of the parent layer"
              }
            },
            "required" => ["parent_layer_id"]
          }
        },
        %{
          "name" => "hfm_store_module_declarations",
          "description" =>
            "Stores module-level declarations (use, require, import, alias) for a module",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "declarations" => %{
                "type" => "array",
                "description" =>
                  "List of declaration lines like ['use GenServer', 'alias Geom.Square as Sq']",
                "items" => "string"
              },
              # "metadata" => %{"type" => "object", "description" => "Optional metadata map", "default" => %{}},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name", "declarations"]
          }
        },
        %{
          "name" => "hfm_get_module_declarations",
          "description" =>
            "Retrieves module declarations from the specified layer, walking up the hierarchy if needed",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name"]
          }
        },
        %{
          "name" => "hfm_store_function",
          "description" => "Stores a function in the specified layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "function_name" => %{"type" => "string", "description" => "Name of the function"},
              "code" => %{"type" => "string", "description" => "Function code"},
              #  "metadata" => %{"type" => "object", "description" => "Optional metadata map", "default" => %{}},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name", "function_name", "code"]
          }
        },
        # %{
        #   "name" => "hfm_store_functions",
        #   "description" => "Stores multiple functions in the specified layer",
        #   "inputSchema" => %{
        #     "type" => "object",
        #     "properties" => %{
        #       "functions" => %{
        #         "type" => "array",
        #         "description" => "List of {module_name, function_name, code, metadata} tuples",
        #         "items" => %{"type" => "object"}
        #       },
        #       "layer_id" => %{"type" => ["integer", "string"], "description" => "Layer ID or :head for current head", "default" => ":head"}
        #     },
        #     "required" => ["functions"]
        #   }
        # },
        %{
          "name" => "hfm_get_function",
          "description" =>
            "Retrieves a function from the specified layer, walking up the hierarchy if needed",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "function_name" => %{"type" => "string", "description" => "Name of the function"},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name", "function_name"]
          }
        },
        %{
          "name" => "hfm_render_module",
          "description" =>
            "Renders a complete module by collecting all functions and declarations for the specified module",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{
                "type" => "string",
                "description" => "Name of the module to render"
              },
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name"]
          }
        },
        %{
          "name" => "hfm_render_all_modules",
          "description" => "Renders all modules visible from the specified layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_list_functions",
          "description" => "Lists all functions visible from the specified layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_list_module_declarations",
          "description" => "Lists all module declarations visible from the specified layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_get_layer_chain",
          "description" => "Gets the complete inheritance chain for a layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_get_head_layer",
          "description" => "Gets the current head layer ID",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{},
            "required" => []
          }
        },
        %{
          "name" => "hfm_list_layers",
          "description" => "Lists all layer IDs in order from head to root",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{},
            "required" => []
          }
        },
        %{
          "name" => "hfm_delete_function",
          "description" =>
            "Deletes a function from the specified layer (creates a tombstone that masks the function in parent layers)",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "function_name" => %{"type" => "string", "description" => "Name of the function"},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name", "function_name"]
          }
        },
        %{
          "name" => "hfm_delete_module_declarations",
          "description" =>
            "Deletes module declarations from the specified layer (creates a tombstone that masks the declarations in parent layers)",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "module_name" => %{"type" => "string", "description" => "Name of the module"},
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => ["module_name"]
          }
        },
        %{
          "name" => "hfm_get_stats",
          "description" => "Gets statistics about the function storage system",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{},
            "required" => []
          }
        },
        %{
          "name" => "hfm_compact_layer",
          "description" =>
            "Compacts a layer by removing functions that are identical to their parent layer",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "layer_id" => %{
                "type" => ["integer", "string"],
                "description" => "Layer ID or :head for current head",
                "default" => ":head"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_save_to_disk",
          "description" => "Saves all layers to disk using ETS tab2file",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "base_path" => %{
                "type" => "string",
                "description" => "Base path for saving layers",
                "default" => "./layers"
              }
            },
            "required" => []
          }
        },
        %{
          "name" => "hfm_load_from_disk",
          "description" =>
            "Loads all layers from disk using ETS file2tab (replaces current state)",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "base_path" => %{
                "type" => "string",
                "description" => "Base path for loading layers",
                "default" => "./layers"
              }
            },
            "required" => []
          }
        }
      ]
    }

    {:ok, tools}
  end

  def dispatch_method("tools/call", %{
        "arguments" => %{"code" => code},
        "name" => "eval_elixir_snippet"
      }) do
    res =
      try do
        Code.eval_string(code)
      catch
        a, b ->
          {:crash, a, b, __STACKTRACE__}
      end

    reply = %{
      "content" => [
        %{
          "text" => inspect(res, pretty: true),
          "type" => "text"
        }
      ]
    }

    {:ok, reply}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_create_layer", "arguments" => args}) do
    %{"parent_layer_id" => parent_layer_id} = args

    case HierarchicalFunctionManager.create_layer(HierarchicalFunctionManager, parent_layer_id) do
      {:ok, layer_id} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Created new layer #{layer_id} with parent #{parent_layer_id}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error creating layer: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{
        "name" => "hfm_store_module_declarations",
        "arguments" => args
      }) do
    %{"module_name" => module_name, "declarations" => declarations} = args
    metadata = Map.get(args, "metadata", %{})
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.store_module_declarations(
           HierarchicalFunctionManager,
           module_name,
           declarations,
           metadata,
           layer_id
         ) do
      {:ok, identifiers} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Stored module declarations for #{module_name}: #{inspect(identifiers)}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error storing module declarations: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{
        "name" => "hfm_get_module_declarations",
        "arguments" => args
      }) do
    %{"module_name" => module_name} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.get_module_declarations(
           HierarchicalFunctionManager,
           module_name,
           layer_id
         ) do
      {:ok, {declarations, metadata, found_layer_id}} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" =>
                 "Module declarations for #{module_name} (found in layer #{found_layer_id}):\nDeclarations: #{inspect(declarations, pretty: true)}\nMetadata: #{inspect(metadata, pretty: true)}"
             }
           ]
         }}

      {:error, :not_found} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Module declarations not found for #{module_name}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_store_function", "arguments" => args}) do
    %{"module_name" => module_name, "function_name" => function_name, "code" => code} = args
    metadata = Map.get(args, "metadata", %{})
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.store_function(
           HierarchicalFunctionManager,
           module_name,
           function_name,
           code,
           metadata,
           layer_id
         ) do
      {:ok, function_names} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" =>
                 "Stored function #{module_name}.#{function_name}: #{inspect(function_names)}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error storing function: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_store_functions", "arguments" => args}) do
    %{"functions" => functions} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.store_functions(
           HierarchicalFunctionManager,
           functions,
           layer_id
         ) do
      {:ok, function_names} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Stored #{length(functions)} functions: #{inspect(function_names)}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error storing functions: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_get_function", "arguments" => args}) do
    %{"module_name" => module_name, "function_name" => function_name} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.get_function(
           HierarchicalFunctionManager,
           module_name,
           function_name,
           layer_id
         ) do
      {:ok, {code, metadata, found_layer_id}} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" =>
                 "Function #{module_name}.#{function_name} (found in layer #{found_layer_id}):\n#{code}\n\nMetadata: #{inspect(metadata, pretty: true)}"
             }
           ]
         }}

      {:error, :not_found} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Function #{module_name}.#{function_name} not found"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_render_module", "arguments" => args}) do
    %{"module_name" => module_name} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.render_module(
           HierarchicalFunctionManager,
           module_name,
           layer_id
         ) do
      {:ok, module_code} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Rendered module #{module_name}:\n\n#{module_code}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error rendering module: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_render_all_modules", "arguments" => args}) do
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.render_all_modules(HierarchicalFunctionManager, layer_id) do
      {:ok, modules} ->
        rendered =
          modules
          |> Enum.map(fn {module_name, module_code} ->
            "=== #{module_name} ===\n#{module_code}"
          end)
          |> Enum.join("\n\n")

        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Rendered #{map_size(modules)} modules:\n\n#{rendered}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error rendering modules: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_list_functions", "arguments" => args}) do
    layer_id = Map.get(args, "layer_id", :head)

    functions = HierarchicalFunctionManager.list_functions(HierarchicalFunctionManager, layer_id)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" =>
             "Functions visible from layer #{layer_id}:\n#{inspect(functions, pretty: true)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{
        "name" => "hfm_list_module_declarations",
        "arguments" => args
      }) do
    layer_id = Map.get(args, "layer_id", :head)

    declarations =
      HierarchicalFunctionManager.list_module_declarations(HierarchicalFunctionManager, layer_id)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" =>
             "Module declarations visible from layer #{layer_id}:\n#{inspect(declarations, pretty: true)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_get_layer_chain", "arguments" => args}) do
    layer_id = Map.get(args, "layer_id", :head)

    chain = HierarchicalFunctionManager.get_layer_chain(HierarchicalFunctionManager, layer_id)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" => "Layer chain from #{layer_id}: #{inspect(chain)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_get_head_layer", "arguments" => _args}) do
    head_layer = HierarchicalFunctionManager.get_head_layer(HierarchicalFunctionManager)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" => "Current head layer: #{inspect(head_layer)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_list_layers", "arguments" => _args}) do
    layers = HierarchicalFunctionManager.list_layers(HierarchicalFunctionManager)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" => "All layers (head to root): #{inspect(layers)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_delete_function", "arguments" => args}) do
    %{"module_name" => module_name, "function_name" => function_name} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.delete_function(
           HierarchicalFunctionManager,
           module_name,
           function_name,
           layer_id
         ) do
      :ok ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Deleted function #{module_name}.#{function_name} from layer #{layer_id}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error deleting function: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{
        "name" => "hfm_delete_module_declarations",
        "arguments" => args
      }) do
    %{"module_name" => module_name} = args
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.delete_module_declarations(
           HierarchicalFunctionManager,
           module_name,
           layer_id
         ) do
      :ok ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Deleted module declarations for #{module_name} from layer #{layer_id}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error deleting module declarations: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_get_stats", "arguments" => _args}) do
    stats = HierarchicalFunctionManager.get_stats(HierarchicalFunctionManager)

    {:ok,
     %{
       "content" => [
         %{
           "type" => "text",
           "text" => "HFM Statistics:\n#{inspect(stats, pretty: true)}"
         }
       ]
     }}
  end

  def dispatch_method("tools/call", %{"name" => "hfm_compact_layer", "arguments" => args}) do
    layer_id = Map.get(args, "layer_id", :head)

    case HierarchicalFunctionManager.compact_layer(layer_id) do
      {:ok, compacted_count} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" =>
                 "Compacted layer #{layer_id}, removed #{compacted_count} redundant functions"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error compacting layer: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_save_to_disk", "arguments" => args}) do
    base_path = Map.get(args, "base_path", "./layers")

    case HierarchicalFunctionManager.save_to_disk(base_path) do
      :ok ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Successfully saved all layers to #{base_path}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error saving to disk: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method("tools/call", %{"name" => "hfm_load_from_disk", "arguments" => args}) do
    base_path = Map.get(args, "base_path", "./layers")

    case HierarchicalFunctionManager.load_from_disk(base_path) do
      :ok ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Successfully loaded all layers from #{base_path}"
             }
           ]
         }}

      {:error, reason} ->
        {:ok,
         %{
           "content" => [
             %{
               "type" => "text",
               "text" => "Error loading from disk: #{inspect(reason)}"
             }
           ]
         }}
    end
  end

  def dispatch_method(method, _params) do
    {:error, -32601, "Method not found: #{method}"}
  end

  def create_error_response(id, code, message) do
    %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => code,
        "message" => message
      },
      "id" => id
    }
  end

  def test() do
    module_name = "Module1"
    declarations = [%{"name" => "value"}]

    {:ok, res} =
      dispatch_method("tools/call", %{
        "name" => "hfm_store_module_declarations",
        "arguments" => %{"module_name" => module_name, "declarations" => declarations}
      })

    %{
      "content" => [
        %{
          "text" => "Stored module declarations for Module1: [\"Module1.__declarations__\"]",
          "type" => "text"
        }
      ]
    } = res

    function = "f1"
    code = "def my_code do 1 end"

    {:ok, res} =
      dispatch_method("tools/call", %{
        "name" => "hfm_store_function",
        "arguments" => %{
          "module_name" => module_name,
          "function_name" => function,
          "code" => code
        }
      })

    %{
      "content" => [
        %{
          "text" => "Stored function Module1.f1: [\"Module1.f1\"]",
          "type" => "text"
        }
      ]
    } = res

    args = %{
      "arguments" => %{
        "declarations" => [
          "use Ecto.Schema",
          "import Ecto.Changeset",
          "alias MyApp.Repo as Repo"
        ],
        "module_name" => "MyApp.User"
      },
      "name" => "hfm_store_module_declarations"
    }

    {:ok, res} = dispatch_method("tools/call", args)

    args = %{
      "arguments" => %{},
      "name" => "hfm_list_module_declarations"
    }

    {:ok, res} = dispatch_method("tools/call", args)

    args = %{
      "arguments" => %{
        "module_name" => "MyApp.User"
      },
      "name" => "hfm_render_module"
    }

    {:ok, res} = dispatch_method("tools/call", args)
  end
end
