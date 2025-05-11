defmodule Nova.Compiler.CodeGenTest do
  use ExUnit.Case, async: false

  alias Nova.Compiler.Ast
  alias Nova.Compiler.CodeGen
  alias Nova.Compiler.Tokenizer
  alias Nova.Compiler.Parser

  test "codegen integer addition" do
    src = """
    module Test.Add where

    plus = 40 + 2
    """

    tokens = Tokenizer.tokenize(src)
    {:ok, ast, []} = Parser.parse_module(tokens)

    mlir = Nova.Compiler.CodeGen.Beaver.compile(ast)
    assert mlir =~ "func.func @plus() -> i32"

    llvm = Nova.Compiler.CodeGen.Beaver.to_llvm(ast)
    assert llvm =~ "llvm.func @plus()"
  end
end
