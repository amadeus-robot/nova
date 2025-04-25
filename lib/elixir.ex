defmodule Nova.Compiler.Elixir do
  alias Nova.Compiler.Ast, as: Ast

  @doc """
  Compiles the parsed AST into Elixir code.
  """
  def compile(ast) do
    case ast do
      %Ast.Module{} = module ->
        compile_module(module)

      %Ast.FunctionDeclaration{} = func_decl ->
        compile_function_declaration(func_decl)

      %Ast.TypeDeclaration{} = type_decl ->
        compile_type_declaration(type_decl)

      %Ast.DataType{} = data_type ->
        compile_data_type(data_type)

      %Ast.TypeClass{} = type_class ->
        compile_type_class(type_class)

      %Ast.TypeClassInstance{} = instance ->
        compile_type_class_instance(instance)

      %Ast.ImportDeclaration{} = import_decl ->
        compile_import_declaration(import_decl)

      %Ast.ForeignImport{} = foreign_import ->
        compile_foreign_import(foreign_import)

      expression ->
        compile_expression(expression)
    end
  end

  # Module compilation
  defp compile_module(%Ast.Module{name: name, declarations: declarations}) do
    module_name = String.replace(name, ".", "_")

    compiled_declarations =
      declarations
      |> Enum.map(&compile/1)
      |> Enum.join("\n\n")

    """
    defmodule #{module_name} do
      #{compiled_declarations}
    end
    """
  end

  # Type declaration - in Elixir, we can't really represent these directly
  # We'll use @type specs as a closest approximation
  defp compile_type_declaration(%Ast.TypeDeclaration{name: name, type_signature: type_signature}) do
    type = compile_type(type_signature.type)
    "@type #{name} :: #{type}"
  end

  # Function declaration
  defp compile_function_declaration(%Ast.FunctionDeclaration{
         name: name,
         parameters: parameters,
         body: body,
         type_signature: type_signature
       }) do
    # Optional type spec if type_signature is provided
    type_spec =
      if type_signature do
        "  @spec #{name}(#{compile_function_params_types(type_signature)}) :: #{compile_type(type_signature.type)}\n"
      else
        ""
      end

    params = Enum.join(parameters, ", ")
    compiled_body = compile_expression(body)

    # Indent body properly for better readability
    indented_body =
      compiled_body
      |> String.split("\n")
      |> Enum.map(fn line -> "    " <> line end)
      |> Enum.join("\n")

    """
      #{type_spec}  def #{name}(#{params}) do
    #{indented_body}
    end
    """
  end

  defp compile_function_params_types(type_signature) do
    # This implementation is simplified and would need to be expanded
    # to properly extract parameter types from function type signatures
    "any"
  end

  # Data type - in Elixir, we'll represent these as structs
  defp compile_data_type(%Ast.DataType{
         name: name,
         type_vars: _type_vars,
         constructors: constructors
       }) do
    # For simplicity, we'll convert ADTs to a single struct with a :type field
    # and one struct per constructor
    constructor_defs =
      constructors
      |> Enum.map(&compile_data_constructor(&1, name))
      |> Enum.join("\n\n")

    """
    #{constructor_defs}
    """
  end

  defp compile_data_constructor(
         %Ast.DataConstructor{name: constructor_name, fields: fields},
         type_name
       ) do
    # Convert fields into struct fields
    field_defs =
      fields
      |> Enum.with_index()
      |> Enum.map(fn {_field, idx} -> ":field_#{idx}" end)
      |> Enum.join(", ")

    """
    defmodule #{type_name}.#{constructor_name} do
      defstruct [#{field_defs}]
    end

    def #{String.downcase(constructor_name)}(#{Enum.map(1..length(fields), fn idx -> "arg_#{idx}" end) |> Enum.join(", ")}) do
      %#{type_name}.#{constructor_name}{#{Enum.map(1..length(fields), fn idx -> "field_#{idx - 1}: arg_#{idx}" end) |> Enum.join(", ")}}
    end
    """
  end

  # Type class - in Elixir, we'll use protocols as an approximation
  defp compile_type_class(%Ast.TypeClass{name: name, methods: methods}) do
    method_specs =
      methods
      |> Enum.map(&compile_type_class_method/1)
      |> Enum.join("\n\n")

    """
    defprotocol #{name} do
      #{method_specs}
    end
    """
  end

  defp compile_type_class_method(%Ast.TypeSignature{name: name, type: type}) do
    """
    @spec #{name}(t) :: #{compile_type(type)}
    def #{name}(value)
    """
  end

  # Type class instance - in Elixir, we'll use protocol implementations
  defp compile_type_class_instance(%Ast.TypeClassInstance{
         class_name: class_name,
         type: type,
         methods: methods
       }) do
    type_name = compile_type(type)

    compiled_methods =
      methods
      |> Enum.map(&compile_function_declaration/1)
      |> Enum.join("\n\n")

    """
    defimpl #{class_name}, for: #{type_name} do
      #{compiled_methods}
    end
    """
  end

  # Import declaration
  defp compile_import_declaration(%Ast.ImportDeclaration{module: module, imports: _imports}) do
    "alias #{module}"
  end

  # Foreign import declaration - direct call to Elixir functions
  defp compile_foreign_import(%Ast.ForeignImport{
         module: module,
         function: function,
         alias: alias_name
       }) do
    """
    def #{alias_name}(args) do
      apply(#{module}, #{function}, args)
    end
    """
  end

  # Expressions
  def compile_expression(expression) do
    case expression do
      %Ast.FunctionCall{function: function, arguments: arguments} ->
        compile_function_call(function, arguments)

      %Ast.BinaryOp{op: op, left: left, right: right} ->
        compile_binary_op(op, left, right)

      %Ast.UnaryOp{op: op, value: value} ->
        compile_unary_op(op, value)

      %Ast.IfExpression{condition: condition, then_branch: then_branch, else_branch: else_branch} ->
        compile_if_expression(condition, then_branch, else_branch)

      %Ast.CaseExpression{expression: expr, cases: cases} ->
        compile_case_expression(expr, cases)

      %Ast.LetBinding{bindings: bindings, body: body} ->
        compile_let_binding(bindings, body)

      %Ast.DoBlock{expressions: expressions} ->
        compile_do_block(expressions)

      %Ast.Lambda{parameters: parameters, body: body} ->
        compile_lambda(parameters, body)

      %Ast.List{elements: elements} ->
        compile_list(elements)

      %Ast.ListComprehension{expression: expression, generators: generators, guards: guards} ->
        compile_list_comprehension(expression, generators, guards)

      %Ast.Tuple{elements: elements} ->
        compile_tuple(elements)

      %Ast.Identifier{name: name} ->
        compile_identifier(name)

      %Ast.Literal{type: type, value: value} ->
        compile_literal(type, value)

      value when is_binary(value) ->
        # String literals from parse_string_literal
        "\"#{String.replace(value, "\"", "\\\"")}\""

      _ ->
        "# Unsupported expression: #{inspect(expression)}"
    end
  end

  defp compile_function_call(function, arguments) when is_binary(function) do
    # Handle special operators and built-in functions
    case function do
      ":" ->
        # This is the cons operator for lists in Nova
        if length(arguments) == 2 do
          head = compile_expression(Enum.at(arguments, 0))
          tail = compile_expression(Enum.at(arguments, 1))
          "[#{head} | #{tail}]"
        else
          # Fallback for incorrect usage
          "cons_operator_error(#{Enum.map(arguments, &compile_expression/1) |> Enum.join(", ")})"
        end

      "++" ->
        # String concatenation (assuming Nova uses ++ for strings too)
        if length(arguments) == 2 do
          left = compile_expression(Enum.at(arguments, 0))
          right = compile_expression(Enum.at(arguments, 1))
          "#{left} <> #{right}"
        else
          # Fallback for incorrect usage
          "concat_error(#{Enum.map(arguments, &compile_expression/1) |> Enum.join(", ")})"
        end

      "toString" ->
        # Convert to string
        if length(arguments) == 1 do
          arg = compile_expression(Enum.at(arguments, 0))
          "to_string(#{arg})"
        else
          # Fallback for incorrect usage
          "to_string_error(#{Enum.map(arguments, &compile_expression/1) |> Enum.join(", ")})"
        end

      _ ->
        # Regular function call
        compiled_args =
          arguments
          |> Enum.map(&compile_expression/1)
          |> Enum.join(", ")

        "#{function}(#{compiled_args})"
    end
  end

  defp compile_function_call(function, arguments) do
    compiled_function = compile_expression(function)

    compiled_args =
      arguments
      |> Enum.map(&compile_expression/1)
      |> Enum.join(", ")

    "(#{compiled_function}).(#{compiled_args})"
  end

  defp compile_binary_op(op, left, right) do
    compiled_left = compile_expression(left)
    compiled_right = compile_expression(right)

    # Handle special cases for operators
    case op do
      "->" ->
        # This is generally used for function types, but can map to anonymous functions
        "fn #{compiled_left} -> #{compiled_right} end"

      _ ->
        "(#{compiled_left} #{op} #{compiled_right})"
    end
  end

  defp compile_unary_op(op, value) do
    compiled_value = compile_expression(value)
    "#{op}(#{compiled_value})"
  end

  defp compile_if_expression(condition, then_branch, else_branch) do
    compiled_condition = compile_expression(condition)
    compiled_then = compile_expression(then_branch)
    compiled_else = compile_expression(else_branch)

    # Indent properly for better readability
    then_indented =
      compiled_then
      |> String.split("\n")
      |> Enum.map(fn line -> "      " <> line end)
      |> Enum.join("\n")

    else_indented =
      compiled_else
      |> String.split("\n")
      |> Enum.map(fn line -> "      " <> line end)
      |> Enum.join("\n")

    """
    if #{compiled_condition} do
    #{then_indented}
    else
    #{else_indented}
    end
    """
  end

  defp compile_case_expression(expr, cases) do
    compiled_expr = compile_expression(expr)

    compiled_cases =
      cases
      |> Enum.map(&compile_case_clause/1)
      |> Enum.join("\n      ")

    """
    case #{compiled_expr} do
      #{compiled_cases}
    end
    """
  end

  defp compile_case_clause(%Ast.CaseClause{pattern: pattern, body: body}) do
    compiled_pattern = compile_pattern(pattern)
    compiled_body = compile_expression(body)

    "#{compiled_pattern} -> #{compiled_body}"
  end

  defp compile_pattern(pattern) do
    case pattern do
      %Ast.FunctionCall{function: constructor, arguments: args} ->
        # For pattern matching on constructors
        # Check if this is a list cons pattern [head | tail]
        if constructor == ":" do
          # This is the cons operator for lists in Nova, make it Elixir-style
          "[#{compile_pattern(List.first(args))} | #{compile_pattern(List.last(args))}]"
        else
          # For ADT constructors
          args_patterns = args |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
          "%#{constructor}{#{args_patterns}}"
        end

      %Ast.Tuple{elements: elements} ->
        elements_patterns = elements |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
        "{#{elements_patterns}}"

      %Ast.List{elements: elements} ->
        elements_patterns = elements |> Enum.map(&compile_pattern/1) |> Enum.join(", ")
        "[#{elements_patterns}]"

      %Ast.Identifier{name: name} ->
        name

      %Ast.Literal{type: type, value: value} ->
        compile_literal(type, value)

      value when is_binary(value) ->
        "\"#{String.replace(value, "\"", "\\\"")}\""

      _ ->
        "# Unsupported pattern: #{inspect(pattern)}"
    end
  end

  defp compile_let_binding(bindings, body) do
    # Generate let bindings, ensuring proper variable scoping in Elixir
    compiled_bindings =
      bindings
      |> Enum.map(fn {name, value} ->
        "#{name} = #{compile_expression(value)}"
      end)
      |> Enum.join("\n")

    compiled_body = compile_expression(body)

    """
    (fn ->
      #{compiled_bindings}
      #{compiled_body}
    end).()
    """
  end

  defp compile_do_block(expressions) do
    compiled_exprs =
      expressions
      |> Enum.map(fn
        {:let, name, value} ->
          "#{name} = #{compile_expression(value)}"

        {:bind, pattern, value} ->
          "#{compile_pattern(pattern)} <- #{compile_expression(value)}"

        {:expr, expr} ->
          compile_expression(expr)
      end)
      |> Enum.join("\n")

    """
    do
      #{compiled_exprs}
    end
    """
  end

  defp compile_lambda(parameters, body) do
    params = parameters |> Enum.map(&compile_parameter/1) |> Enum.join(", ")
    compiled_body = compile_expression(body)

    "fn #{params} -> #{compiled_body} end"
  end

  defp compile_parameter(param) do
    case param do
      %Ast.Identifier{name: name} -> name
      _ -> compile_pattern(param)
    end
  end

  defp compile_list(elements) do
    compiled_elements = elements |> Enum.map(&compile_expression/1) |> Enum.join(", ")
    "[#{compiled_elements}]"
  end

  defp compile_list_comprehension(expression, generators, guards) do
    compiled_expr = compile_expression(expression)

    compiled_generators =
      generators
      |> Enum.map(fn %Ast.Generator{pattern: pattern, expression: expr} ->
        "#{compile_pattern(pattern)} <- #{compile_expression(expr)}"
      end)
      |> Enum.join(", ")

    compiled_guards =
      guards
      |> Enum.map(&compile_expression/1)
      |> Enum.join(", ")

    guard_clause = if length(guards) > 0, do: ", #{compiled_guards}", else: ""

    "for #{compiled_generators}#{guard_clause}, do: #{compiled_expr}"
  end

  defp compile_tuple(elements) do
    compiled_elements = elements |> Enum.map(&compile_expression/1) |> Enum.join(", ")
    "{#{compiled_elements}}"
  end

  defp compile_identifier(name) do
    name
  end

  defp compile_literal(type, value) do
    case type do
      :number -> "#{value}"
      :string -> "\"#{String.replace(value, "\"", "\\\"")}\""
      :char -> "'#{value}'"
      :boolean -> "#{value}"
      _ -> "#{value}"
    end
  end

  # Type compilation - converting Nova types to Elixir type specs
  defp compile_type(type) do
    case type do
      %Ast.Identifier{name: name} ->
        name

      %Ast.FunctionCall{function: "[]", arguments: [elem_type]} ->
        "[#{compile_type(elem_type)}]"

      %Ast.FunctionCall{function: function, arguments: args} ->
        args_types = args |> Enum.map(&compile_type/1) |> Enum.join(", ")
        "#{function}(#{args_types})"

      %Ast.BinaryOp{op: "->", left: left, right: right} ->
        "(#{compile_type(left)} -> #{compile_type(right)})"

      %Ast.Tuple{elements: elements} ->
        elements_types = elements |> Enum.map(&compile_type/1) |> Enum.join(", ")
        "{#{elements_types}}"

      _ ->
        "any"
    end
  end
end
