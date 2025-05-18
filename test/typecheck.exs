defmodule Nova.TypeCheckerTest do
  use ExUnit.Case, async: true

  alias Nova.Compiler.{Ast, TypeChecker, Types}
  alias Types.{Env}

  # Helper builders -----------------------------------------------------------
  defp lit_num(n), do: %Ast.Literal{type: :number, value: Integer.to_string(n)}
  defp lit_str(s), do: %Ast.Literal{type: :string, value: s}
  defp ident(name), do: %Ast.Identifier{name: name}
  defp lambda(param, body), do: %Ast.Lambda{parameters: [ident(param)], body: body}
  defp call(fun, args), do: %Ast.FunctionCall{function: fun, arguments: args}
  defp bin(op, l, r), do: %Ast.BinaryOp{op: op, left: l, right: r}
  defp let(bindings, body), do: %Ast.LetBinding{bindings: bindings, body: body}

  setup do
    {:ok, env: Env.empty()}
  end

  describe "literal inference" do
    test "number literals are Int", %{env: env} do
      {:ok, t, _, _} = TypeChecker.infer_expr(lit_num(42), env)
      assert t == Types.t_int()
    end

    test "string literals are String", %{env: env} do
      {:ok, t, _, _} = TypeChecker.infer_expr(lit_str("hi"), env)
      assert t == Types.t_string()
    end
  end

  describe "lambda and application" do
    test "\\x -> x + 1 has type Int -> Int", %{env: env} do
      expr = lambda("x", bin("+", ident("x"), lit_num(1)))
      {:ok, t, _, _} = TypeChecker.infer_expr(expr, env)
      assert t == Types.t_arrow(Types.t_int(), Types.t_int())
    end

    test "(\\x -> x)(5) infers Int", %{env: env} do
      expr = call(lambda("x", ident("x")), [lit_num(5)])
      {:ok, t, _, _} = TypeChecker.infer_expr(expr, env)
      assert t == Types.t_int()
    end
  end

  describe "let‑binding" do
    test "polymorphic like identity is reused", %{env: env} do
      # let id = \x -> x in id 5
      id_lambda = lambda("x", ident("x"))
      expr = let([{"id", id_lambda}], call(ident("id"), [lit_num(5)]))
      {:ok, t, _, _} = TypeChecker.infer_expr(expr, env)
      assert t == Types.t_int()
    end
  end

  describe "binary operators" do
    test "equality returns Bool", %{env: env} do
      expr = bin("==", lit_num(3), lit_num(4))
      {:ok, t, _, _} = TypeChecker.infer_expr(expr, env)
      assert t == Types.t_bool()
    end

    test "arithmetic coerces to Int", %{env: env} do
      expr = bin("*", lit_num(2), lit_num(5))
      {:ok, t, _, _} = TypeChecker.infer_expr(expr, env)
      assert t == Types.t_int()
    end
  end

  describe "function declaration & module" do
    test "simple function declaration adds to env" do
      add_one_decl = %Ast.FunctionDeclaration{
        name: "addOne",
        parameters: [ident("n")],
        body: bin("+", ident("n"), lit_num(1)),
        type_signature: nil
      }

      mod = %Ast.Module{name: ident("Test"), declarations: [add_one_decl]}

      {:ok, env2} = TypeChecker.check_module(mod)
      {:ok, scheme} = Types.Env.lookup(env2, "addOne")
      add_one_t = Types.t_arrow(Types.t_int(), Types.t_int())
      # Instantiate to compare
      assert TypeChecker.send(:instantiate, [scheme, env2]) |> elem(0) == add_one_t
    end
  end

  describe "error conditions" do
    test "unbound identifier raises", %{env: env} do
      assert {:error, _} = TypeChecker.infer_expr(ident("nope"), env)
    end

    test "type mismatch in binary op", %{env: env} do
      expr = bin("+", lit_num(1), lit_str("hey"))
      assert {:error, _} = TypeChecker.infer_expr(expr, env)
    end
  end
end
