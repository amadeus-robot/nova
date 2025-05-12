defmodule Nova.Compiler.WasmGenerator do
  alias Nova.Compiler.Ast

  def generate(module_ast) do
    """
    (module
      #{generate_declarations(module_ast.declarations)}
    )
    """
  end

  defp generate_declarations(decls) do
    decls
    |> Enum.map(&generate_declaration/1)
    |> Enum.join("\n")
  end

  defp generate_declaration(%Ast.FunctionDeclaration{name: name, parameters: params, body: body}) do
    "(func $#{name} (param #{generate_params(params)}) (result i32) #{generate_expression(body)})"
  end

  defp generate_declaration(_), do: ""

  defp generate_params(params) do
    params
    # assume all params are i32 for now
    |> Enum.map(fn _ -> "i32" end)
    |> Enum.join(" ")
  end

  defp generate_expression(%Ast.Literal{type: :int, value: value}) do
    "(i32.const #{value})"
  end

  defp generate_expression(%Ast.FunctionCall{
         function: %Ast.Identifier{name: fn_name},
         arguments: args
       }) do
    args_code = Enum.map(args, &generate_expression/1) |> Enum.join(" ")
    "(call $#{fn_name} #{args_code})"
  end

  defp generate_expression(%Ast.BinaryOp{op: "+", left: left, right: right}) do
    "#{generate_expression(left)} #{generate_expression(right)} (i32.add)"
  end

  defp generate_expression(_), do: "(i32.const 0)"
end
