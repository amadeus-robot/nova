defmodule Nova.Compiler.CodeGen.ForeignTest do
  use ExUnit.Case
  alias Nova.Compiler.{Ast, CodeGen}

  # Build minimal AST directly (tokenizer/parser not needed for this demo)
  @delete_type %Ast.TypeSignature{
    name: "delete",
    type_vars: [],
    constraints: [],
    # [a] -> a -> [a]
    type: %Ast.BinaryOp{
      op: "->",
      left: %Ast.FunctionCall{function: "[]", arguments: [%Ast.Identifier{name: "a"}]},
      right: %Ast.BinaryOp{
        op: "->",
        left: %Ast.Identifier{name: "a"},
        right: %Ast.FunctionCall{function: "[]", arguments: [%Ast.Identifier{name: "a"}]}
      }
    }
  }

  @foreign %Ast.ForeignImport{
    module: "Elixir.List",
    function: "delete",
    alias: "delete",
    type_signature: @delete_type
  }

  @remove3 %Ast.FunctionDeclaration{
    name: "remove3",
    parameters: [%Ast.Identifier{name: "xs"}],
    body: %Ast.FunctionCall{
      function: %Ast.Identifier{name: "delete"},
      arguments: [%Ast.Identifier{name: "xs"}, %Ast.Literal{type: :number, value: "3"}]
    },
    type_signature: nil
  }

  test "foreign import wrapper forwards to List.delete/2" do
    ast_mod = %Ast.Module{name: "TestForeign", declarations: [@foreign, @remove3]}
    src = CodeGen.compile(ast_mod)
    IO.inspect(src)
    [{mod, _}] = Code.compile_string(src)
    assert apply(mod, :remove3, [[1, 3, 4, 3]]) == [1, 4, 3]
  end
end
