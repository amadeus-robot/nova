defmodule Nova.Compiler.CodeGen.Beaver do
  alias Beaver.{MLIR, Dialects, Composer}
  alias Beaver.Dialects.{Arith, Func, LLVM}

  def to_llvm(%Nova.Compiler.Ast.Module{} = m) do
    m
    |> compile()
    |> Beaver.MLIR.Parser.parse!()
    |> Composer.nested("func.func", "llvm-request-c-wrappers")
    # arith → llvm, func → llvm, etc.
    |> Composer.run!()
    |> MLIR.Module.to_string()
  end

  @doc """
  Compile a Nova `Ast.Module` to MLIR text.
  """
  def compile(%Nova.Compiler.Ast.Module{} = mod) do
    ctx = MLIR.Context.create()
    # ⇽ makes arith/func available
    MLIR.Context.load_all_dialects(ctx)

    # ✅ give it the context
    loc = MLIR.Location.unknown(ctx)
    ir_mod = MLIR.Module.create(ctx, loc)

    Enum.each(mod.declarations, &emit_decl(&1, ir_mod, ctx, loc))

    MLIR.Module.to_string(ir_mod)
  end

  # ---------- tiny pattern-match codegen -------------
  # Only FunctionDeclaration with a single expression body right now.
  defp emit_decl(
         %Nova.Compiler.Ast.FunctionDeclaration{name: fun, parameters: [], body: body},
         ir_mod,
         ctx,
         loc
       ) do
    ftype = Func.type(ctx, [], Arith.i32(ctx))
    func = Func.build(ir_mod, "#{fun}", ftype)

    entry = MLIR.Block.append(func)
    MLIR.Block.set_insertion(entry)

    value = emit_expr(body, ctx, loc)
    Func.return([value])
  end

  # ---------- expressions ----------
  defp emit_expr(%Nova.Compiler.Ast.Literal{type: :number, value: int}, ctx, _loc) do
    Arith.constant(Arith.i32(ctx), int)
  end

  defp emit_expr(
         %Nova.Compiler.Ast.BinaryOp{op: "+", left: l, right: r},
         ctx,
         loc
       ) do
    lval = emit_expr(l, ctx, loc)
    rval = emit_expr(r, ctx, loc)
    Arith.addi(lval, rval)
  end

  defp emit_expr(%Nova.Compiler.Ast.Identifier{name: id}, _ctx, _loc) do
    # look up SSA value in current block (todo – env threading)
    MLIR.Value.lookup!(id)
  end

  # Fallback – raise so tests remind you to extend coverage
  defp emit_expr(ast, _ctx, _loc),
    do: raise("Beaver codegen does not support #{inspect(ast)} yet")
end
