defmodule TH1 do
  def go0 do
    source = File.read!("lib/ast.nv")
    tokens = Nova.Compiler.Tokenizer.tokenize(source)
    {:ok, all, rest} = Nova.Compiler.Parser.parse_declarations(tokens)
    IO.inspect(rest)
    all
  end

  def go() do
    source = File.read!("lib/parser.nv")
    tokens = Nova.Compiler.Tokenizer.tokenize(source)
    {:ok, all, []} = Nova.Compiler.Parser.parse_declarations(tokens)
    funcs = Enum.filter(all, fn x -> x.__struct__ == Nova.Compiler.Ast.TypeSignature end)

    childs =
      Enum.map(funcs, fn x ->
        %Nova.Compiler.Ast.TypeSignature{
          name: name,
          type_vars: _,
          constraints: _,
          type: _
        } = x

        %{
          uid: name,
          title: name,
          content: "create the functions for this module",
          children: [],
          status: "Waiting"
        }
      end)

    root =
      [
        %{
          uid: "Nova.Compiler.Parser",
          title: "Nova.Compiler.Parser",
          content: "create the functions for this module",
          children: childs,
          status: "Waiting"
        }
      ]

    out = JSON.encode!(root)

    File.write!("tasks.json", out)
  end
end
