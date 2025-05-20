defmodule Nova.Compiler.Parser do
  alias Nova.Compiler.Tokenizer.Token
  alias Nova.Compiler.Ast, as: Ast

  # ------------------------------------------------------------
  #  Helpers for newline‑aware token handling
  # ------------------------------------------------------------
  defp skip_newlines(tokens) do
    Enum.drop_while(tokens, fn %Token{type: t} -> t == :newline end)
  end

  defp drop_newlines([%Token{type: :newline} | rest]), do: drop_newlines(rest)
  defp drop_newlines(tokens), do: tokens

  defp strip_newlines(list),
    do:
      Enum.reject(list, fn
        %Token{type: :newline} -> true
        _ -> false
      end)

  defp ensure_consumed(rest) do
    case skip_newlines(rest) do
      [] ->
        :ok

      leftover ->
        tok = hd(leftover)

        {:error,
         "unexpected tokens after successful parse – " <>
           "#{tok.type}:#{inspect(tok.value)} at line #{tok.line}, col #{tok.column}"}
    end
  end

  def parse_declarations(tokens), do: parse_declarations(tokens, [])
  def parse_declarations([], acc), do: {:ok, Enum.reverse(acc), []}

  def parse_declarations(tokens, acc) do
    tokens = skip_newlines(tokens)

    case parse_declaration(tokens) do
      {:ok, decl, rest} -> parse_declarations(rest, [decl | acc])
      {:error, _} when acc != [] -> {:ok, Enum.reverse(acc), tokens}
      other -> other
    end
  end

  # ------------------------------------------------------------
  #  Declarations (import, data, function, …)
  # ------------------------------------------------------------
  def parse_declaration(tokens) do
    # try function+signature combo first
    case parse_function_with_type_signature(tokens) do
      {:ok, v, rest} ->
        {:ok, v, rest}

      {:error, _} ->
        case parse_type_signature(tokens) do
          {:ok, ts, rest} ->
            {:ok, ts, rest}

          {:error, _} ->
            parse_any(
              [
                &parse_module_header/1,
                &parse_import/1,
                &parse_foreign_import_simple/1,
                &parse_foreign_import/1,
                &parse_data_declaration/1,
                &parse_type_alias/1,
                &parse_type_class/1,
                &parse_type_class_instance/1,
                &parse_function_declaration/1
              ],
              tokens
            )
        end
    end
  end

  # ------------------------------------------------------------
  #  Module parsing
  # ------------------------------------------------------------
  def parse_module(tokens) do
    with {:ok, module_ast, tokens} <- parse_module_header(tokens),
         {:ok, decls, rest} <- parse_declarations(tokens),
         :ok <- ensure_consumed(rest) do
      {:ok, %{module_ast | declarations: decls}, []}
    end
  end

  # Internal helper used when `module` appears as a declaration
  defp parse_module_header(tokens) do
    tokens = drop_newlines(tokens)

    with {:ok, _, tokens} <- expect_keyword(tokens, "module"),
         {:ok, module_name, tokens} <- parse_qualified_identifier(tokens),
         {:ok, _, tokens} <- expect_keyword(tokens, "where") do
      {:ok, %Ast.Module{name: module_name}, tokens}
    else
      other -> other
    end
  end

  # ------------------------------------------------------------
  #  Import
  # ------------------------------------------------------------
  defp parse_import(tokens) do
    tokens = drop_newlines(tokens)

    with {:ok, _, tokens} <- expect_keyword(tokens, "import"),
         {:ok, mod, tokens} <- parse_qualified_identifier(tokens),
         {alias, tokens} <- parse_import_alias(tokens),
         {:ok, items, hiding?, tokens} <- parse_import_selectors(tokens) do
      {:ok,
       %Ast.ImportDeclaration{
         module: mod,
         alias: alias,
         items: items,
         hiding?: hiding?
       }, drop_newlines(tokens)}
    end
  end

  # as  <ident>
  defp parse_import_alias([%Token{type: :identifier, value: "as"} | rest]) do
    with {:ok, id, rest} <- parse_identifier(rest) do
      {id.name, rest}
    end
  end

  defp parse_import_alias(tokens), do: {nil, tokens}

  #    ("(" … ")" | "hiding" "(" … ")" | ε)
  defp parse_import_selectors([%Token{type: :identifier, value: "hiding"} | rest]) do
    with {:ok, items, rest} <- parse_paren_import_list(rest) do
      {:ok, items, true, rest}
    end
  end

  defp parse_import_selectors(tokens) do
    case parse_paren_import_list(tokens) do
      {:ok, items, rest} -> {:ok, items, false, rest}
      {:error, _} -> {:ok, [], false, tokens}
    end
  end

  # "(" ImportList ")"
  defp parse_paren_import_list([%Token{type: :delimiter, value: "("} | rest]) do
    with {:ok, items, rest} <-
           parse_separated(&parse_import_item/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, ")") do
      {:ok, items, rest}
    end
  end

  defp parse_paren_import_list(_), do: {:error, "no paren import list"}

  # Foo | Foo(..) | Foo(Bar,Baz)
  defp parse_import_item(tokens) do
    with {:ok, first, tokens} <- parse_identifier(tokens) do
      case tokens do
        [%Token{type: :delimiter, value: "("} | _] ->
          # keep the "(" so parse_constructors/2 matches its head clause
          parse_constructors(tokens, first.name)

        _ ->
          {:ok, first.name, tokens}
      end
    end
  end

  defp parse_constructors([%Token{type: :delimiter, value: "("} | rest], mod) do
    case rest do
      [%Token{type: :operator, value: "."}, %Token{type: :operator, value: "."} | rest2] ->
        {:ok, {mod, :all}, expect_delimiter(rest2, ")") |> elem(2)}

      [%Token{type: :operator, value: ".."} | rest2] ->
        {:ok, {mod, :all}, expect_delimiter(rest2, ")") |> elem(2)}

      _ ->
        with {:ok, ctors, rest2} <-
               parse_separated(&parse_identifier/1, &expect_delimiter(&1, ","), rest),
             {:ok, _, rest3} <- expect_delimiter(rest2, ")") do
          {:ok, {mod, Enum.map(ctors, & &1.name)}, rest3}
        end
    end
  end

  # ------------------------------------------------------------
  #  Foreign import (elixir)
  # ------------------------------------------------------------
  defp parse_foreign_import_simple(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "foreign"),
         {:ok, _, tokens} <- expect_keyword(tokens, "import"),
         {:ok, name, tokens} <- parse_identifier(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "::"),
         {:ok, type, tokens} <- parse_type(tokens) do
      {:ok,
       %Ast.ForeignImport{
         # not specified
         module: nil,
         # not specified
         function: nil,
         # the identifier itself
         alias: name.name,
         type_signature: %Ast.TypeSignature{
           name: name.name,
           type_vars: [],
           constraints: [],
           type: type
         }
       }, drop_newlines(tokens)}
    else
      _ -> {:error, "plain foreign import parse failed"}
    end
  end

  defp parse_foreign_import(tokens) do
    tokens = drop_newlines(tokens)

    with {:ok, _, tokens} <- expect_keyword(tokens, "foreign"),
         {:ok, _, tokens} <- expect_keyword(tokens, "import"),
         # ⬇︎ target language is an identifier (keep it if you ever need it)
         {:ok, _lang, tokens} <- parse_identifier(tokens),
         {:ok, mod, tokens} <- parse_string_literal(tokens),
         {:ok, fun, tokens} <- parse_string_literal(tokens),
         {:ok, al, tokens} <- parse_identifier(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "::"),
         {:ok, type, tokens} <- parse_type(tokens) do
      {:ok,
       %Ast.ForeignImport{
         module: mod,
         function: fun,
         alias: al.name,
         type_signature: %Ast.TypeSignature{
           name: al.name,
           type_vars: [],
           constraints: [],
           type: type
         }
       }, tokens}
    else
      other -> other
    end
  end

  defp parse_record_pattern([%Token{type: :delimiter, value: "{"} | rest]) do
    with {:ok, fields, rest} <-
           parse_separated(&parse_record_field_pattern/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "}") do
      {:ok, %Ast.RecordPattern{fields: fields}, rest}
    end
  end

  defp parse_record_pattern(_), do: {:error, "Expected record pattern"}

  # ------------------------------------------------------------------
  # Record-pattern field:   { head       }  |  { head = h }
  #                         { head : tok }  |  { head = tok }
  # ------------------------------------------------------------------
  defp parse_record_field_pattern(tokens) do
    with {:ok, lbl, tokens} <- parse_label(tokens) do
      cond do
        # ──────────────── `:` delimiter ────────────────
        match?({:ok, _, _}, expect_delimiter(tokens, ":")) ->
          {:ok, _, tokens} = expect_delimiter(tokens, ":")

          with {:ok, pat, tokens} <- parse_pattern(tokens) do
            {:ok, {lbl.name, pat}, tokens}
          end

        # ──────────────── `=` operator ────────────────
        match?({:ok, _, _}, expect_operator(tokens, "=")) ->
          {:ok, _, tokens} = expect_operator(tokens, "=")

          with {:ok, pat, tokens} <- parse_pattern(tokens) do
            {:ok, {lbl.name, pat}, tokens}
          end

        # ─────────── shorthand  { head } ───────────────
        true ->
          {:ok, {lbl.name, %Ast.Identifier{name: lbl.name}}, tokens}
      end
    end
  end

  # Data type declaration
  defp parse_data_declaration(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "data"),
         {:ok, type_name, tokens} <- parse_identifier(tokens),
         {:ok, type_vars, tokens} <- parse_many(&parse_identifier/1, tokens),
         tokens = skip_newlines(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "="),
         tokens = skip_newlines(tokens),
         {:ok, constructors, tokens} <- parse_data_constructors(tokens) do
      {:ok,
       %Ast.DataType{
         name: type_name.name,
         type_vars: Enum.map(type_vars, fn var -> var.name end),
         constructors: constructors
       }, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_data_constructors(tokens) do
    parse_separated(&parse_data_constructor/1, &expect_operator(&1, "|"), tokens)
  end

  defp parse_data_constructor([%Token{column: base} | _] = tokens) do
    with {:ok, ctor_name, tokens1} <- parse_identifier(tokens) do
      tokens1 = skip_newlines(tokens1)

      cond do
        # ❶   Constructor { … }
        match?([%Token{type: :delimiter, value: "{"} | _], tokens1) ->
          with {:ok, fields, tokens2} <- parse_braced_record_fields(tokens1) do
            {:ok, %Ast.DataConstructor{name: ctor_name.name, fields: fields, record?: true},
             tokens2}
          end

        # ❷   Constructor LF  indented-fields
        (case tokens1 do
           [%Token{type: :newline} | rest] ->
             rest = skip_newlines(rest)

           [%Token{column: col} | _] when col > base ->
             true

           _ ->
             false
         end) ->
          # cursor is still at tokens1 (newline or first field)
          {_, after_newlines} = split_until_newline(tokens1)

          with {:ok, fields, tokens2} <- collect_layout_record_fields(after_newlines, [], base) do
            {:ok, %Ast.DataConstructor{name: ctor_name.name, fields: fields, record?: true},
             tokens2}
          end

        # ❸   Ordinary positional constructor
        true ->
          with {:ok, field_types, tokens2} <- parse_many(&parse_type_atom/1, tokens1) do
            {:ok, %Ast.DataConstructor{name: ctor_name.name, fields: field_types, record?: false},
             tokens2}
          end
      end
    end
  end

  # ───────────────────────────────────────────────────────────────
  #  Record-constructor field:  label :: Type
  # ───────────────────────────────────────────────────────────────
  defp parse_record_constructor_field(tokens) do
    with {:ok, label_ast, tokens} <- parse_identifier(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "::"),
         tokens = skip_newlines(tokens),
         {:ok, type_ast, tokens} <- parse_type(tokens) do
      {:ok, %Ast.DataField{label: label_ast.name, type: type_ast}, tokens}
    end
  end

  # Collect “label :: Type, …” between braces  { … }
  defp parse_braced_record_fields([%Token{type: :delimiter, value: "{"} | rest]) do
    with {:ok, fields, rest} <-
           parse_separated(&parse_record_constructor_field/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "}") do
      {:ok, fields, rest}
    end
  end

  defp parse_braced_record_fields(_), do: {:error, "Expected '{' for record constructor"}

  # Layout-based (indent) list – stop when we dedent or hit “|”
  defp collect_layout_record_fields([%Token{type: :newline} | rest], acc, base) do
    rest = skip_newlines(rest)

    case rest do
      [%Token{column: col} | _] when col > base ->
        with {:ok, field, rest} <- parse_record_constructor_field(rest) do
          collect_layout_record_fields(rest, [field | acc], base)
        end

      _ ->
        {:ok, Enum.reverse(acc), rest}
    end
  end

  defp collect_layout_record_fields(tokens, acc, _base), do: {:ok, Enum.reverse(acc), tokens}

  # Skip superclass constraints in a `class` declaration – everything
  # up to (and including) the first "<=" operator is treated as
  # constraints and ignored for now. Returns `{rest, constraints_tokens}`.
  defp skip_superclass_constraints(tokens) do
    tokens = skip_newlines(tokens)

    {before, after_} =
      Enum.split_while(tokens, fn
        %Token{type: :operator, value: "<="} -> false
        _ -> true
      end)

    case after_ do
      [%Token{type: :operator, value: "<="} | rest] -> {rest, before}
      _ -> {tokens, []}
    end
  end

  # If a class header contains a kind annotation, consume it and return the
  # parsed type. The annotation is optional and currently stored verbatim.
  defp maybe_parse_class_kind([%Token{type: :operator, value: "::"} | rest]) do
    with {:ok, kind, rest} <- parse_type(rest) do
      {rest, kind}
    end
  end

  defp maybe_parse_class_kind(tokens), do: {tokens, nil}

  # Drop leading instance constraints – identical idea but we only
  # need the remainder of the tokens (constraints are ignored for now).
  defp drop_instance_constraints(tokens) do
    {_before, after_} =
      Enum.split_while(tokens, fn
        %Token{type: :operator, value: "<="} -> false
        _ -> true
      end)

    case after_ do
      [%Token{type: :operator, value: "<="} | rest] -> rest
      _ -> tokens
    end
  end

  # ────────────────────────────────────────────────────
  # UPDATED PARSERS
  # ────────────────────────────────────────────────────
  # Supports superclass constraints:  
  #   class (Applicative m, Bind m) <= Monad m where …
  defp parse_type_class(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "class") do
      {tokens, _constraints} = skip_superclass_constraints(tokens)

      with {:ok, class_name, tokens} <- parse_identifier(tokens),
           {tokens, kind} <- maybe_parse_class_kind(tokens),
           {:ok, type_vars, tokens} <- parse_many(&parse_identifier/1, tokens) do
        tokens = skip_newlines(tokens)

        case tokens do
          [%Token{type: :keyword, value: "where"} | rest] ->
            with {:ok, methods, rest} <- parse_many(&parse_type_signature/1, rest) do
              {:ok,
               %Ast.TypeClass{
                 name: class_name.name,
                 type_vars: Enum.map(type_vars, & &1.name),
                 methods: methods,
                 kind: kind
               }, rest}
            end

          _ ->
            {:ok,
             %Ast.TypeClass{
               name: class_name.name,
               type_vars: Enum.map(type_vars, & &1.name),
               methods: [],
               kind: kind
             }, tokens}
        end
      end
    end
  end

  # Handles both named and unnamed instances, with optional constraints:
  #   instance showString :: Show String where …
  #   instance (Eq a) <= Show a where …
  defp parse_type_class_instance(tokens) do
    {tokens, derived?} =
      case tokens do
        [%Token{type: :keyword, value: "derive"} | rest] -> {rest, true}
        _ -> {tokens, false}
      end

    with {:ok, _, tokens} <- expect_keyword(tokens, "instance") do
      tokens = drop_newlines(tokens)

      case tokens do
        # ── Named instance – starts with ident followed by '::' ──
        [%Token{type: :identifier, value: inst_name}, %Token{type: :operator, value: "::"} | rest] ->
          rest = drop_instance_constraints(rest)

          with {:ok, type_ast, tokens} <- parse_type(rest),
               {:ok, _, tokens} <- expect_keyword(tokens, "where"),
               {:ok, methods, tokens} <- parse_many(&parse_function_declaration/1, tokens) do
            class_name =
              case type_ast do
                %Ast.FunctionCall{function: %Ast.Identifier{name: cn}} -> cn
                %Ast.Identifier{name: cn} -> cn
                _ -> inst_name
              end

            {:ok,
             %Ast.TypeClassInstance{
               class_name: class_name,
               type: type_ast,
               methods: methods,
               derived?: derived?
             }, tokens}
          end

        # ── Unnamed instance – old syntax: Class Type where … ──
        _ ->
          tokens = drop_instance_constraints(tokens)

          with {:ok, class_name, tokens} <- parse_identifier(tokens),
               {:ok, type_ast, tokens} <- parse_type(tokens),
               {:ok, _, tokens} <- expect_keyword(tokens, "where"),
               {:ok, methods, tokens} <- parse_many(&parse_function_declaration/1, tokens) do
            {:ok,
             %Ast.TypeClassInstance{
               class_name: class_name.name,
               type: type_ast,
               methods: methods,
               derived?: derived?
             }, tokens}
          end
      end
    end
  end

  # Parse a function declaration with its type signature
  defp split_type_and_rest(tokens, name) do
    Enum.split_while(tokens, fn
      %Token{type: :identifier, value: ^name} -> false
      _ -> true
    end)
  end

  defp parse_function_with_type_signature(tokens) do
    tokens = drop_newlines(tokens)

    case tokens do
      [%Token{type: :identifier, value: name} | rest1] ->
        with {:ok, _, rest2} <- expect_operator(rest1, "::"),
             {type_tokens, rest3} <- split_type_and_rest(rest2, name),
             {:ok, type_ast, []} <- parse_type(strip_newlines(type_tokens)),
             {:ok, fun_ast, final} <- parse_function_declaration(rest3),
             true <- fun_ast.name == name do
          {:ok,
           %Ast.FunctionDeclaration{
             name: fun_ast.name,
             parameters: fun_ast.parameters,
             body: fun_ast.body,
             type_signature: %Ast.TypeSignature{
               name: name,
               type_vars: [],
               constraints: [],
               type: type_ast
             }
           }, final}
        else
          _ -> {:error, "function-with-signature parse failed"}
        end

      _ ->
        {:error, "Expected identifier at start of type signature"}
    end
  end

  # Type signature parsing
  defp parse_type_signature(tokens) do
    tokens = drop_newlines(tokens)

    with {:ok, name, tokens} <- parse_identifier(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "::"),
         {:ok, type, tokens} <- parse_type(tokens) do
      {:ok, %Ast.TypeSignature{name: name.name, type_vars: [], constraints: [], type: type},
       tokens}
    else
      other -> other
    end
  end

  # Type parsing
  defp parse_type([%Token{type: :identifier, value: "forall"} | _] = toks),
    do: parse_forall_type(toks)

  defp parse_type(toks), do: parse_function_type(toks)

  # forall a b.  ty
  defp parse_forall_type([%Token{value: "forall"} | rest]) do
    with {:ok, vars, rest} <- parse_many(&parse_identifier/1, rest),
         {:ok, _, rest} <- expect_operator(rest, "."),
         {:ok, ty, rest} <- parse_type(rest) do
      {:ok, %Ast.ForAllType{vars: Enum.map(vars, & &1.name), type: ty}, rest}
    end
  end

  defp parse_type_alias(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "type"),
         {:ok, name, tokens} <- parse_identifier(tokens),
         {:ok, vars, tokens} <- parse_many(&parse_identifier/1, tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "="),
         tokens = skip_newlines(tokens),
         {:ok, aliased, tokens} <- parse_type(tokens) do
      {:ok,
       %Ast.TypeAlias{
         name: name.name,
         type_vars: Enum.map(vars, & &1.name),
         type: aliased
       }, tokens}
    else
      other -> other
    end
  end

  defp parse_function_type(tokens) do
    with {:ok, left, tokens} <- parse_type_term(tokens) do
      case tokens do
        [%Token{type: :operator, value: "->"} | rest] ->
          with {:ok, right, rest} <- parse_function_type(rest) do
            {:ok, %Ast.BinaryOp{op: "->", left: left, right: right}, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # 2. Parser helpers 
  def parse_record_type([%Token{type: :delimiter, value: "{"} | rest]) do
    with {:ok, fields, rest} <-
           parse_separated(&parse_record_field/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "}") do
      {:ok, %Ast.RecordType{fields: fields}, rest}
    end
  end

  def parse_record_type(_), do: {:error, "Expected record type"}

  defp parse_record_field(tokens) do
    with {:ok, label, tokens} <- parse_label(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "::"),
         tokens = skip_newlines(tokens),
         {:ok, t, tokens} <- parse_type(tokens) do
      {:ok, {label.name, t}, tokens}
    end
  end

  defp parse_type_term(tokens) do
    parse_any(
      [
        &parse_record_type/1,
        &parse_list_type/1,
        &parse_tuple_type/1,
        &parse_basic_type/1
      ],
      tokens
    )
  end

  defp parse_list_type(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "["} | rest] ->
        with {:ok, element_type, rest} <- parse_type(rest),
             {:ok, _, rest} <- expect_delimiter(rest, "]") do
          {:ok, %Ast.FunctionCall{function: "[]", arguments: [element_type]}, rest}
        end

      _ ->
        {:error, "Expected list type"}
    end
  end

  defp parse_tuple_type(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "("} | rest] ->
        with {:ok, elements, rest} <-
               parse_separated(&parse_type/1, &expect_delimiter(&1, ","), rest),
             {:ok, _, rest} <- expect_delimiter(rest, ")") do
          if length(elements) == 1 do
            # Parentheses used only for grouping → return the inner type
            {:ok, List.first(elements), rest}
          else
            # Real tuple type
            {:ok, %Ast.Tuple{elements: elements}, rest}
          end
        end

      _ ->
        {:error, "Expected tuple type"}
    end
  end

  # Modified to fix the application type parsing
  defp parse_basic_type(tokens) do
    case parse_qualified_identifier(tokens) do
      {:ok, qid, rest} ->
        # Gather any type arguments that follow the qualified name
        case parse_many(&parse_type_atom/1, rest) do
          {:ok, [], ^rest} ->
            {:ok, qid, rest}

          {:ok, args, new_rest} ->
            {:ok, %Ast.FunctionCall{function: qid, arguments: args}, new_rest}
        end

      # fall back to the old rules
      _ ->
        parse_basic_type_fallback(tokens)
    end
  end

  defp parse_basic_type_fallback(tokens) do
    case tokens do
      [%Token{type: :identifier, value: name} | rest] ->
        # Parse type arguments if any
        case parse_many(&parse_type_atom/1, rest) do
          {:ok, [], ^rest} ->
            # No arguments, just an identifier
            {:ok, %Ast.Identifier{name: name}, rest}

          {:ok, args, new_rest} ->
            # Type with arguments
            {:ok, %Ast.FunctionCall{function: name, arguments: args}, new_rest}
        end

      [%Token{type: :delimiter, value: "("} | rest] ->
        with {:ok, type, rest} <- parse_type(rest),
             {:ok, _, rest} <- expect_delimiter(rest, ")") do
          {:ok, type, rest}
        end

      _ ->
        {:error, "Expected basic type"}
    end
  end

  # Parse a type atom (used for application args)
  def parse_type_atom(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "{"} | _] = toks ->
        parse_record_type(toks)

      [%Token{type: :identifier, value: name} | _] = toks ->
        parse_qualified_identifier(toks)

      [%Token{type: :delimiter, value: "("} | rest] ->
        with {:ok, type, rest} <- parse_type(rest),
             {:ok, _, rest} <- expect_delimiter(rest, ")") do
          {:ok, type, rest}
        end

      [%Token{type: :delimiter, value: "["} | rest] ->
        with {:ok, element_type, rest} <- parse_type(rest),
             {:ok, _, rest} <- expect_delimiter(rest, "]") do
          {:ok, %Ast.FunctionCall{function: "[]", arguments: [element_type]}, rest}
        end

      _ ->
        {:error, "Expected type atom"}
    end
  end

  # ------------------------------------------------------------
  #  Function declaration (no type signature)                   
  # ------------------------------------------------------------
  defp parse_function_declaration(tokens) do
    with {:ok, name, tokens} <- parse_identifier(tokens),
         {:ok, parameters, tokens} <- parse_function_parameters(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "="),
         # remember top-level column for layout checks
         [%Token{column: indent} | _] = tokens,
         {:ok, raw_body, tokens} <- parse_expression(tokens),
         # ← NEW: look for an optional where-clause
         {:ok, body, tokens} <- maybe_parse_where(tokens, indent, raw_body) do
      {:ok,
       %Ast.FunctionDeclaration{
         name: name.name,
         # ← keep full pattern nodes
         parameters: parameters,
         body: body,
         type_signature: nil
       }, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_function_parameters(tokens), do: parse_many(&parse_simple_pattern/1, tokens)

  # ------------------------------------------------------------
  #  Simple pattern (for parameters)
  # ------------------------------------------------------------
  defp parse_simple_pattern(tokens) do
    parse_any(
      [
        &parse_literal/1,
        &parse_identifier/1,
        &parse_tuple_pattern/1,
        &parse_list_pattern/1,
        fn
          [%Token{type: :delimiter, value: "("} | rest] ->
            with {:ok, pattern, rest} <- parse_pattern(rest),
                 {:ok, _, rest} <- expect_delimiter(rest, ")") do
              {:ok, pattern, rest}
            end

          _ ->
            {:error, "Expected parenthesized pattern"}
        end
      ],
      tokens
    )
  end

  defp maybe_parse_where(tokens, _outer_indent, body) do
    tokens = skip_newlines(tokens)

    case tokens do
      # grab the column where "where" begins
      [%Token{type: :keyword, value: "where", column: where_col} | rest0] ->
        rest0 = skip_newlines(rest0)

        # first binding must indent strictly more than where_col
        [%Token{column: col_first} | _] = rest0
        true = col_first > where_col

        {:ok, bindings, rest} = collect_where_bindings(rest0, where_col, [])
        {:ok, %Ast.LetBinding{bindings: bindings, body: body}, rest}

      _ ->
        {:ok, body, tokens}
    end
  end

  defp collect_where_bindings(tokens, where_col, acc) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{column: col} | _] when col > where_col ->
        {:ok, bind, rest} = parse_binding(tokens)
        collect_where_bindings(rest, where_col, [bind | acc])

      _ ->
        {:ok, Enum.reverse(acc), tokens}
    end
  end

  # Pattern parsing for other contexts like case clauses
  def parse_pattern(tokens) do
    parse_any(
      [
        &parse_record_pattern/1,
        &parse_wildcard_pattern/1,
        &parse_cons_pattern/1,
        &parse_constructor_pattern/1,
        &parse_tuple_pattern/1,
        &parse_list_pattern/1,
        &parse_literal/1,
        &parse_identifier/1,
        fn tokens ->
          case tokens do
            [%Token{type: :delimiter, value: "("} | rest] ->
              with {:ok, pattern, rest} <- parse_pattern(rest),
                   {:ok, _, rest} <- expect_delimiter(rest, ")") do
                {:ok, pattern, rest}
              end

            _ ->
              {:error, "Expected parenthesized pattern"}
          end
        end
      ],
      tokens
    )
  end

  defp capital?(<<c, _::binary>>) when c in ?A..?Z, do: true
  defp capital?(_), do: false

  defp parse_constructor_pattern(tokens) do
    with {:ok, ctor, rest} <- parse_identifier(tokens),
         # <── NEW line
         true <- capital?(ctor.name),
         {:ok, args, rest} <- parse_many(&parse_pattern/1, rest) do
      case args do
        [] ->
          {:ok, %Ast.Identifier{name: ctor.name}, rest}

        _ ->
          {:ok,
           %Ast.FunctionCall{
             function: %Ast.Identifier{name: ctor.name},
             arguments: args
           }, rest}
      end
    else
      # fall-through
      _ -> {:error, "Expected constructor pattern"}
    end
  end

  defp parse_cons_pattern(tokens) do
    with {:ok, head, tokens} <- parse_simple_pattern(tokens),
         # the ":" itself
         {:ok, _, tokens} <- expect_colon(tokens),
         {:ok, tail, tokens} <- parse_pattern(tokens) do
      {:ok,
       %Ast.FunctionCall{
         function: %Ast.Identifier{name: ":"},
         arguments: [head, tail]
       }, tokens}
    else
      _ -> {:error, "Expected cons pattern"}
    end
  end

  defp expect_colon([%Token{value: ":"} | rest]), do: {:ok, ":", rest}
  defp expect_colon(_), do: {:error, "Expected ':'"}

  def parse_wildcard_pattern([%Token{type: :identifier, value: "_"} | rest]),
    do: {:ok, %Ast.Wildcard{}, rest}

  def parse_wildcard_pattern(_), do: {:error, "Expected wildcard"}

  defp parse_tuple_pattern(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "("} | rest] ->
        with {:ok, elements, rest} <-
               parse_separated(&parse_pattern/1, &expect_delimiter(&1, ","), rest),
             {:ok, _, rest} <- expect_delimiter(rest, ")") do
          {:ok, %Ast.Tuple{elements: elements}, rest}
        end

      _ ->
        {:error, "Expected tuple pattern"}
    end
  end

  defp parse_list_pattern([
         %Token{type: :delimiter, value: "["},
         %Token{type: :delimiter, value: "]"} | rest
       ]) do
    {:ok, %Ast.List{elements: []}, rest}
  end

  # ── pattern: [p1, p2, …] ────────────────────────────────────────
  defp parse_list_pattern([%Token{type: :delimiter, value: "["} | rest]) do
    with {:ok, elements, rest} <-
           parse_separated(&parse_pattern/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "]") do
      {:ok, %Ast.List{elements: elements}, rest}
    end
  end

  defp parse_list_pattern(_), do: {:error, "Expected list pattern"}

  # Lambda expression parsing
  defp parse_lambda(tokens) do
    with {:ok, _, tokens} <- expect_operator(tokens, "\\"),
         {:ok, parameters, tokens} <- parse_function_parameters(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "->"),
         {:ok, body, tokens} <- parse_expression(tokens) do
      {:ok, %Ast.Lambda{parameters: parameters, body: body}, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Expression parsing
  def parse_expression(tokens) do
    parse_binary_expression(tokens)
  end

  # ------------------------------------------------------------
  #  Let-expression  (layout-aware)
  # ------------------------------------------------------------
  def parse_let_expression(tokens) do
    tokens = skip_newlines(tokens)

    with {:ok, _, tokens} <- expect_keyword(tokens, "let"),
         # gap A
         tokens = skip_newlines(tokens),
         {:ok, bindings, tokens} <- parse_many(&parse_binding/1, tokens),
         # gap B
         tokens = skip_newlines(tokens),
         {:ok, _, tokens} <- expect_keyword(tokens, "in"),
         # gap C
         tokens = skip_newlines(tokens),
         {:ok, body, tokens} <- parse_expression(tokens) do
      {:ok, %Ast.LetBinding{bindings: bindings, body: body}, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # ------------------------------------------------------------
  #  Single binding  (accepts its own leading newline)
  # ------------------------------------------------------------
  def parse_binding(tokens) do
    tokens = skip_newlines(tokens)

    # ──  try “name params … = rhs” first ──────────────────────
    case parse_function_declaration(tokens) do
      {:ok, %Ast.FunctionDeclaration{} = fun, rest} ->
        # desugar into a lambda so the RHS is still an *expression*
        lambda = %Ast.Lambda{parameters: fun.parameters, body: fun.body}
        {:ok, {%Ast.Identifier{name: fun.name}, lambda}, rest}

      _ ->
        # ── fall back to ordinary pattern = rhs ───────────────
        with {:ok, pat, tokens} <- parse_pattern(tokens),
             {:ok, _, tokens} <- expect_operator(tokens, "="),
             {:ok, rhs, tokens} <- parse_expression(tokens) do
          {:ok, {pat, rhs}, tokens}
        end
    end
  end

  defp parse_if_expression(tokens) do
    tokens = skip_newlines(tokens)

    with {:ok, _, tokens} <- expect_keyword(tokens, "if"),
         {:ok, condition, tokens} <- parse_expression(tokens),
         {:ok, _, tokens} <- expect_keyword(tokens, "then"),
         {:ok, then_branch, tokens} <- parse_expression(tokens),
         {:ok, _, tokens} <- expect_keyword(tokens, "else"),
         {:ok, else_branch, tokens} <- parse_expression(tokens) do
      {:ok,
       %Ast.IfExpression{
         condition: condition,
         then_branch: then_branch,
         else_branch: else_branch
       }, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Helper to check if a token sequence likely starts a new pattern
  defp is_pattern_start?(tokens) do
    case tokens do
      # Empty tokens can't start a pattern
      [] -> false
      # Number literals often start patterns
      [%Token{type: :number} | _] -> true
      # Identifiers often start patterns
      [%Token{type: :identifier} | _] -> true
      # Delimiters like (, [, { can start patterns
      [%Token{type: :delimiter, value: v} | _] when v in ["(", "[", "{"] -> true
      # String and char literals can be patterns
      [%Token{type: :string} | _] -> true
      [%Token{type: :char} | _] -> true
      # Keywords that might terminate a case expression
      [%Token{type: :keyword, value: val} | _] when val in ["end", "in", "else", "then"] -> true
      # Otherwise it's not a pattern start
      _ -> false
    end
  end

  def parse_case_expression(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "case"),
         {:ok, expr, tokens} <- parse_expression(tokens),
         # ⭐ new
         tokens = skip_newlines(tokens),
         {:ok, _, tokens} <- expect_keyword(tokens, "of") do
      parse_case_clauses(tokens, expr, [])
    else
      other -> other
    end
  end

  # Parse case clauses recursively, collecting all clauses
  def parse_case_clauses(tokens, expression, acc) do
    # Try to parse a single case clause
    case parse_case_clause(tokens) do
      {:ok, clause, remaining} ->
        # Successfully parsed a clause, continue with the remaining tokens
        parse_case_clauses(remaining, expression, [clause | acc])

      {:error, _} when acc != [] ->
        # If parsing fails but we've already got some clauses, we're done
        {:ok, %Ast.CaseExpression{expression: expression, cases: Enum.reverse(acc)}, tokens}

      {:error, reason} ->
        # If parsing fails and we have no clauses, propagate the error
        {:error, reason}
    end
  end

  defp split_until_newline(tokens) do
    Enum.split_while(tokens, fn
      %Token{type: :newline} -> false
      _ -> true
    end)
  end

  def clause_start?(tokens) do
    # "pattern  ->"  without consuming the tokens
    with {:ok, _pat, rest} <- parse_pattern(tokens),
         {_, _guard, rest} <- maybe_parse_guard(rest),
         {:ok, _arrow, _} <- expect_operator(rest, "->") do
      true
    else
      _ -> false
    end
  end

  defp take_body(tokens, acc, indent) do
    case tokens do
      [] ->
        {Enum.reverse(acc), []}

      [%Token{type: :newline} = nl | rest] ->
        rest = skip_newlines(rest)

        clause_start = clause_start?(rest)

        case rest do
          # ❶ plain dedent – always end the body
          [%Token{column: col} | _] = next when col < indent ->
            {Enum.reverse(acc), next}

          # ❷ same-column clause start – also end the body
          [%Token{column: col} | _] = next when col == indent and clause_start ->
            {Enum.reverse(acc), next}

          # ❸ otherwise keep accumulating
          _ ->
            take_body(rest, [nl | acc], indent)
        end

      [tok | rest] ->
        take_body(rest, [tok | acc], indent)
    end
  end

  def parse_case_clause(tokens) do
    tokens = skip_newlines(tokens)

    # Record the left-edge column of this clause’s pattern
    case tokens do
      [] ->
        {:error, "no more to parse"}

      [%Token{column: indent} | _] ->
        with {:ok, pattern, tokens} <- parse_pattern(tokens),
             {_, guard, tokens} <- maybe_parse_guard(tokens),
             {:ok, _, tokens} <- expect_operator(tokens, "->") do
          {body_tokens, rest} = take_body(tokens, [], indent)

          with {:ok, body, remaining} <- parse_expression(body_tokens),
               [] <- skip_newlines(remaining) do
            {:ok, %Ast.CaseClause{pattern: pattern, guard: guard, body: body},
             drop_newlines(rest)}
          else
            {:error, reason} -> {:error, reason}
            _ -> {:error, "unexpected tokens after case-clause body"}
          end
        end
    end
  end

  def maybe_parse_guard([%Token{type: :operator, value: "|"} | rest] = all) do
    case parse_expression(rest) do
      {:ok, guard_ast, rest} ->
        {:ok, guard_ast, rest}

      {:error, _} ->
        {:error, nil, all}
    end
  end

  def maybe_parse_guard(tokens), do: {:error, nil, tokens}

  defp parse_do_block(tokens) do
    with {:ok, _, tokens} <- expect_keyword(tokens, "do"),
         {:ok, expressions, tokens} <- parse_many(&parse_do_expression/1, tokens),
         remaining = skip_until_end(tokens) do
      {:ok, %Ast.DoBlock{expressions: expressions}, remaining}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_do_expression(tokens) do
    parse_any(
      [
        fn tokens ->
          with {:ok, _, tokens} <- expect_keyword(tokens, "let"),
               {:ok, name, tokens} <- parse_identifier(tokens),
               {:ok, _, tokens} <- expect_operator(tokens, "="),
               {:ok, value, tokens} <- parse_expression(tokens) do
            {:ok, {:let, name.name, value}, tokens}
          else
            {:error, _} -> {:error, "Expected let binding in do block"}
          end
        end,
        fn tokens ->
          with {:ok, expr, tokens} <- parse_expression(tokens),
               {:ok, _, tokens} <- expect_operator(tokens, "<-"),
               {:ok, value, tokens} <- parse_expression(tokens) do
            {:ok, {:bind, expr, value}, tokens}
          else
            {:error, _} -> {:error, "Expected bind expression in do block"}
          end
        end,
        fn tokens ->
          with {:ok, expr, tokens} <- parse_expression(tokens) do
            {:ok, {:expr, expr}, tokens}
          else
            {:error, _} -> {:error, "Expected expression in do block"}
          end
        end
      ],
      tokens
    )
  end

  # Binary expression parsing with precedence
  # ------------------------------------------------------------
  # 1. top-level binary-expression entry point
  # ------------------------------------------------------------
  defp parse_binary_expression(tokens) do
    parse_any(
      [
        &parse_let_expression/1,
        &parse_if_expression/1,
        &parse_case_expression/1,
        &parse_do_block/1,
        &parse_lambda/1,
        &parse_dollar_expression/1
      ],
      tokens
    )
  end

  defp parse_dollar_expression(tokens) do
    # first parse *anything* tighter than '$'
    with {:ok, left, tokens} <- parse_logical_expression(tokens) do
      tokens = skip_newlines(tokens)

      case tokens do
        [%Token{type: :operator, value: "$"} | rest] ->
          rest = skip_newlines(rest)

          # right-associative: parse the *whole* rhs with the same rule
          with {:ok, right, rest} <- parse_dollar_expression(rest) do
            {:ok, %Ast.FunctionCall{function: left, arguments: [right]}, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # ------------------------------------------------------------
  # 2. logical  (&&  ||)  – lowest precedence
  # ------------------------------------------------------------
  defp parse_logical_expression(tokens) do
    with {:ok, left, tokens} <- parse_comparison_expression(tokens) do
      case tokens do
        [%Token{type: :operator, value: op} | rest] when op in ["&&", "||"] ->
          with {:ok, right, rest} <- parse_logical_expression(rest) do
            {:ok, %Ast.BinaryOp{op: op, left: left, right: right}, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # ------------------------------------------------------------
  # 3. comparison (== != < <= > >=)
  # ------------------------------------------------------------
  defp parse_comparison_expression(tokens) do
    tokens = skip_newlines(tokens)

    with {:ok, left, tokens} <- parse_additive_expression(tokens) do
      tokens = skip_newlines(tokens)

      case tokens do
        [%Token{type: :operator, value: op} | rest]
        when op in ["==", "!=", "/=", "<", "<=", ">", ">="] ->
          rest = skip_newlines(rest)

          with {:ok, right, rest} <- parse_comparison_expression(rest) do
            {:ok, %Ast.BinaryOp{op: op, left: left, right: right}, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # ------------------------------------------------------------
  # 4. additive (+ -)
  # ------------------------------------------------------------
  defp parse_additive_expression(tokens) do
    with {:ok, left, tokens} <- parse_multiplicative_expression(tokens) do
      case tokens do
        [%Token{type: :operator, value: op} | rest] when op in ["+", "-", "++", "<>"] ->
          with {:ok, right, rest} <- parse_additive_expression(rest) do
            {:ok, %Ast.BinaryOp{op: op, left: left, right: right}, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # ── record literal  ─────────────────────────────────────────
  defp parse_record_literal([%Token{type: :delimiter, value: "{"} | rest]) do
    with {:ok, fields, rest} <-
           parse_separated(&parse_record_field_expr/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "}") do
      {:ok, %Ast.RecordLiteral{fields: fields}, rest}
    end
  end

  defp parse_record_literal(_), do: {:error, "Expected record literal"}

  defp parse_record_field_expr(tokens) do
    with {:ok, label, tokens} <- parse_identifier(tokens),
         # uses the new ':'
         {:ok, _, tokens} <- expect_delimiter(tokens, ":"),
         tokens = skip_newlines(tokens),
         {:ok, expr, tokens} <- parse_expression(tokens) do
      {:ok, {label.name, expr}, tokens}
    end
  end

  # ------------------------------------------------------------
  # 5. multiplicative (* /)
  # ------------------------------------------------------------
  defp parse_multiplicative_expression(tokens) do
    # <- was parse_comparison_expression/1
    with {:ok, left, tokens} <- parse_unary_expression(tokens) do
      case tokens do
        [%Token{type: :operator, value: op} | rest] when op in ["*", "/"] ->
          with {:ok, right, rest} <- parse_multiplicative_expression(rest) do
            {:ok, %Ast.BinaryOp{op: op, left: left, right: right}, rest}
          end

        [
          %Token{type: :operator, value: "`"},
          %Token{type: :identifier, value: fun},
          %Token{type: :operator, value: "`"} | rest
        ] ->
          with {:ok, right, rest} <- parse_multiplicative_expression(rest) do
            {:ok,
             %Ast.FunctionCall{
               function: %Ast.Identifier{name: fun},
               arguments: [left, right]
             }, rest}
          end

        _ ->
          {:ok, left, tokens}
      end
    end
  end

  # ------------------------------------------------------------
  # 5-a. unary (-  +  !)          – tighter than multiplicative
  # ------------------------------------------------------------
  defp parse_unary_expression([%Token{type: :operator, value: op} | rest])
       when op in ["-", "+", "!"] do
    with {:ok, expr, rest} <- parse_unary_expression(rest) do
      {:ok, %Ast.UnaryOp{op: op, value: expr}, rest}
    end
  end

  defp parse_unary_expression(tokens), do: parse_application(tokens)

  # Function application parsing
  defp parse_application([%Token{column: base} | _] = toks) do
    with {:ok, fn_term, rest} <- parse_term(toks) do
      {args, rest} = collect_application_args(rest, [], base)

      case args do
        [] -> {:ok, fn_term, rest}
        _ -> {:ok, %Ast.FunctionCall{function: fn_term, arguments: args}, rest}
      end
    end
  end

  defp parse_application([]) do
    {:error, :no_tokens_remaining}
  end

  # Helper function to collect all arguments for function application
  defp collect_application_args([%Token{type: :newline} | rest], acc, base) do
    rest = skip_newlines(rest)

    case rest do
      [%Token{column: col} | _] when col > base ->
        case parse_term(rest) do
          {:ok, arg, rest2} -> collect_application_args(rest2, acc ++ [arg], base)
          # something else - abort
          {:error, _} -> {acc, rest}
        end

      _ ->
        # dedent or EOF → application ends
        {acc, rest}
    end
  end

  defp collect_application_args(tokens, acc, base) do
    case parse_term(tokens) do
      {:ok, arg, rest} -> collect_application_args(rest, acc ++ [arg], base)
      {:error, _} -> {acc, tokens}
    end
  end

  defp parse_term(tokens) do
    parse_any(
      [
        &parse_record_literal/1,
        &parse_literal/1,
        &parse_list_literal/1,
        &parse_list_comprehension/1,
        &parse_tuple_literal/1,
        &parse_qualified_identifier/1,
        fn tokens ->
          case tokens do
            [%Token{type: :delimiter, value: "("} | rest] ->
              with {:ok, expr, rest} <- parse_expression(rest),
                   {:ok, _, rest} <- expect_delimiter(rest, ")") do
                {:ok, expr, rest}
              end

            _ ->
              {:error, "Expected parenthesized expression"}
          end
        end
      ],
      tokens
    )
  end

  # ── expressions: [] literal ─────────────────────────────────────
  defp parse_list_literal([
         %Token{type: :delimiter, value: "["},
         %Token{type: :delimiter, value: "]"} | rest
       ]) do
    {:ok, %Ast.List{elements: []}, rest}
  end

  defp parse_list_literal([%Token{type: :delimiter, value: "["} | rest]) do
    with {:ok, elements, rest} <-
           parse_separated(&parse_expression/1, &expect_delimiter(&1, ","), rest),
         {:ok, _, rest} <- expect_delimiter(rest, "]") do
      {:ok, %Ast.List{elements: elements}, rest}
    end
  end

  defp parse_list_literal(_), do: {:error, "Expected list literal"}

  defp parse_list_comprehension(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "["} | rest] ->
        with {:ok, expression, rest} <- parse_expression(rest),
             {:ok, _, rest} <- expect_operator(rest, "|"),
             {:ok, generators, rest} <-
               parse_separated(&parse_generator/1, &expect_delimiter(&1, ","), rest),
             {:ok, _, rest} <- expect_delimiter(rest, "]") do
          {:ok,
           %Ast.ListComprehension{expression: expression, generators: generators, guards: []},
           rest}
        end

      _ ->
        {:error, "Expected list comprehension"}
    end
  end

  defp parse_generator(tokens) do
    with {:ok, pattern, tokens} <- parse_pattern(tokens),
         {:ok, _, tokens} <- expect_operator(tokens, "<-"),
         {:ok, expression, tokens} <- parse_expression(tokens) do
      {:ok, %Ast.Generator{pattern: pattern, expression: expression}, tokens}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_tuple_literal(tokens) do
    case tokens do
      [%Token{type: :delimiter, value: "("} | rest] ->
        with {:ok, elements, rest} <-
               parse_separated(&parse_expression/1, &expect_delimiter(&1, ","), rest),
             {:ok, _, rest} <- expect_delimiter(rest, ")") do
          if length(elements) == 1 do
            # Single element in parentheses is just grouping, not a tuple
            {:ok, List.first(elements), rest}
          else
            {:ok, %Ast.Tuple{elements: elements}, rest}
          end
        end

      _ ->
        {:error, "Expected tuple literal"}
    end
  end

  defp parse_literal(tokens) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :number, value: value} | rest] ->
        {:ok, %Ast.Literal{type: :number, value: value}, rest}

      [%Token{type: :string, value: value} | rest] ->
        {:ok, %Ast.Literal{type: :string, value: value}, rest}

      [%Token{type: :char, value: value} | rest] ->
        {:ok, %Ast.Literal{type: :char, value: value}, rest}

      _ ->
        {:error, "Expected literal"}
    end
  end

  defp parse_string_literal(tokens) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :string, value: value} | rest] -> {:ok, value, rest}
      _ -> {:error, "Expected string literal"}
    end
  end

  defp parse_label(tokens) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: t, value: name} | rest] when t in [:identifier, :keyword] ->
        {:ok, %Ast.Identifier{name: name}, rest}

      _ ->
        {:error, "Expected label"}
    end
  end

  defp parse_identifier(tokens) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :identifier, value: name} | rest] -> {:ok, %Ast.Identifier{name: name}, rest}
      _ -> {:error, "Expected identifier"}
    end
  end

  defp parse_qualified_identifier(tokens) do
    parse_separated(&parse_identifier/1, &expect_operator(&1, "."), tokens)
    |> case do
      {:ok, [%Ast.Identifier{name: ns}, %Ast.Identifier{name: id}], rest} ->
        {:ok, %Ast.QualifiedIdentifier{namespace: ns, name: id}, rest}

      {:ok, parts, rest} when is_list(parts) ->
        joined = Enum.map(parts, & &1.name) |> Enum.join(".")
        {:ok, %Ast.Identifier{name: joined}, rest}

      other ->
        other
    end
  end

  # Helpers
  defp parse_any(parsers, tokens) do
    case parsers do
      [] ->
        {:error, "No parser succeeded #{inspect(List.first(tokens))}"}

      [parser | rest] ->
        case parser.(tokens) do
          {:ok, result, remaining} -> {:ok, result, remaining}
          {:error, _} -> parse_any(rest, tokens)
        end
    end
  end

  def parse_many(parser, tokens) do
    parse_many(parser, tokens, [])
  end

  def parse_many(parser, tokens, acc) do
    case parser.(tokens) do
      {:ok, result, remaining} ->
        parse_many(parser, remaining, [result | acc])

      {:error, _} ->
        {:ok, Enum.reverse(acc), tokens}
    end
  end

  defp parse_separated(parser, separator, tokens) do
    with {:ok, first, tokens} <- parser.(tokens) do
      parse_separated_rest(parser, separator, tokens, [first])
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_separated_rest(parser, separator, tokens, acc) do
    case separator.(tokens) do
      {:ok, _, tokens} ->
        case parser.(tokens) do
          {:ok, item, rest} ->
            parse_separated_rest(parser, separator, rest, [item | acc])

          {:error, _} ->
            {:error, "Expected item after separator"}
        end

      {:error, _} ->
        # No more separators, we're done
        {:ok, Enum.reverse(acc), tokens}
    end
  end

  def expect_keyword(tokens, expected) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :keyword, value: ^expected} | rest] ->
        {:ok, expected, rest}

      _ ->
        {:error, "Expected keyword '#{expected}'"}
    end
  end

  defp expect_operator(tokens, expected) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :operator, value: ^expected} | rest] ->
        {:ok, expected, rest}

      _ ->
        {:error, "Expected operator '#{expected}'"}
    end
  end

  defp expect_delimiter(tokens, expected) do
    tokens = skip_newlines(tokens)

    case tokens do
      [%Token{type: :delimiter, value: ^expected} | rest] ->
        {:ok, expected, rest}

      _ ->
        {:error, "Expected delimiter '#{expected}'"}
    end
  end

  defp skip_until_end(tokens) do
    case Enum.find_index(tokens, fn token -> token.type == :keyword and token.value == "end" end) do
      nil -> tokens
      idx -> Enum.drop(tokens, idx + 1)
    end
  end
end
