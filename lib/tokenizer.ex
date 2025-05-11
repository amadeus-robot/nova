defmodule Nova.Compiler.Tokenizer do
  defmodule Token do
    defstruct [:type, :value, :line, :column, :pos]
  end

  @keywords ~w(foreign module where import data type class instance let in if then else case of do)
  @operators ~w(
  == != <= >= -> <- :: ++ ++= >>= >> << && || 
 <>  
  + - * / < > = $
  \`
)
  @operators_single_char [?+, ?-, ?*, ?/, ?=, ?<, ?>, ?!, ?:, ?., ?|, ?\\, ?&, ?$, ?`]

  def tokenize(source) when is_binary(source) do
    tokenize(source, [], 1, 1, 0)
  end

  defp tokenize("", acc, _line, _column, _pos) do
    Enum.reverse(acc)
  end

  # New-line (Unix or Windows). We emit a :newline token so the parser
  # can drive layout-sensitive rules later.
  defp tokenize("\r\n" <> rest, acc, line, column, pos) do
    token = %Token{type: :newline, value: "\n", line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line + 1, 1, pos + 2)
  end

  defp tokenize("\n" <> rest, acc, line, column, pos) do
    token = %Token{type: :newline, value: "\n", line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line + 1, 1, pos + 1)
  end

  defp tokenize(<<c::utf8, _::binary>> = input, acc, line, column, pos)
       when c in [?\s, ?\t] do
    {rest, line, column, pos} = consume_hspace(input, line, column, pos)
    tokenize(rest, acc, line, column, pos)
  end

  # Line comment
  # defp tokenize("--" <> rest, acc, line, column, pos) do
  #  {rest, new_line, new_column, new_pos} = consume_line_comment(rest, line, column + 2, pos + 2)
  #  tokenize(rest, acc, new_line, new_column, new_pos)
  # end

  defp tokenize("--" <> rest, acc, line, column, pos) do
    {rest_after_nl, new_line, new_column, new_pos} =
      rest
      |> consume_until_newline(line, column + 2, pos + 2)

    # Start tokenising again *after* the newline
    tokenize(rest_after_nl, acc, new_line, new_column, new_pos)
  end

  # Block comment
  defp tokenize("{-" <> rest, acc, line, column, pos) do
    {rest, new_line, new_column, new_pos} =
      consume_block_comment(rest, line, column + 2, pos + 2, 1)

    tokenize(rest, acc, new_line, new_column, new_pos)
  end

  # String literal
  defp tokenize("\"" <> rest, acc, line, column, pos) do
    {value, rest, new_line, new_column, new_pos} =
      consume_string(rest, "", line, column + 1, pos + 1)

    token = %Token{type: :string, value: value, line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], new_line, new_column + 1, new_pos + 1)
  end

  # 1 regular char  →  'a'
  defp tokenize(<<"'", rest::binary>>, acc, line, column, pos) do
    case consume_char(rest, line, column + 1, pos + 1) do
      {<<>>, _, _, _, _} ->
        # unterminated or bad literal – treat the quote as plain text
        token = %Token{type: :unrecognized, value: "'", line: line, column: column, pos: pos}
        tokenize(rest, [token | acc], line, column + 1, pos + 1)

      {value, rest2, new_line, new_column, new_pos} ->
        token = %Token{type: :char, value: value, line: line, column: column, pos: pos}
        tokenize(rest2, [token | acc], new_line, new_column, new_pos)
    end
  end

  # Number - Integer or Float
  defp tokenize(<<c::utf8, rest::binary>>, acc, line, column, pos) when c >= ?0 and c <= ?9 do
    {value, rest, new_column, new_pos} =
      consume_number(<<c::utf8>> <> rest, "", line, column, pos)

    token = %Token{type: :number, value: value, line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line, new_column, new_pos)
  end

  # Operator
  defp tokenize(<<c::utf8, _::binary>> = input, acc, line, column, pos)
       when c in @operators_single_char do
    {op, rest, new_column, new_pos} = consume_operator(input, line, column, pos)

    # If we only picked up a *single* ':' → treat it as a delimiter
    tok_type = if op == ":", do: :delimiter, else: :operator

    token = %Token{type: tok_type, value: op, line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line, new_column, new_pos)
  end

  # Identifier or keyword
  defp tokenize(<<c::utf8, rest::binary>>, acc, line, column, pos)
       when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or c == ?_ do
    {identifier, rest, new_column, new_pos} =
      consume_identifier(<<c::utf8>> <> rest, "", line, column, pos)

    type = if identifier in @keywords, do: :keyword, else: :identifier
    token = %Token{type: type, value: identifier, line: line, column: column, pos: pos}

    tokenize(rest, [token | acc], line, new_column, new_pos)
  end

  # Delimiters
  defp tokenize(<<c::utf8, rest::binary>>, acc, line, column, pos)
       when c in [?(, ?), ?{, ?}, ?[, ?], ?,, ?;] do
    delimiter = <<c::utf8>>
    token = %Token{type: :delimiter, value: delimiter, line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line, column + 1, pos + 1)
  end

  # Catch unrecognized characters
  defp tokenize(<<c::utf8, rest::binary>>, acc, line, column, pos) do
    unrecognized = <<c::utf8>>
    token = %Token{type: :unrecognized, value: unrecognized, line: line, column: column, pos: pos}
    tokenize(rest, [token | acc], line, column + 1, pos + 1)
  end

  defp consume_until_newline("\n" <> rest, line, _column, pos) do
    {rest, line + 1, 1, pos + 1}
  end

  defp consume_until_newline(<<_::utf8, rest::binary>>, line, column, pos) do
    consume_until_newline(rest, line, column + 1, pos + 1)
  end

  # Helpers to consume specific token types
  defp consume_line_comment("\n" <> rest, line, column, pos) do
    {"\n" <> rest, line, column, pos}
  end

  defp consume_line_comment(<<_::utf8, rest::binary>>, line, column, pos) do
    consume_line_comment(rest, line, column + 1, pos + 1)
  end

  defp consume_line_comment("", line, column, pos) do
    {"", line, column, pos}
  end

  defp consume_block_comment("-}" <> rest, line, column, pos, 1) do
    {rest, line, column + 2, pos + 2}
  end

  defp consume_block_comment("{-" <> rest, line, column, pos, depth) do
    consume_block_comment(rest, line, column + 2, pos + 2, depth + 1)
  end

  defp consume_block_comment("-}" <> rest, line, column, pos, depth) do
    consume_block_comment(rest, line, column + 2, pos + 2, depth - 1)
  end

  defp consume_block_comment("\n" <> rest, line, _column, pos, depth) do
    consume_block_comment(rest, line + 1, 1, pos + 1, depth)
  end

  defp consume_block_comment(<<_::utf8, rest::binary>>, line, column, pos, depth) do
    consume_block_comment(rest, line, column + 1, pos + 1, depth)
  end

  defp consume_block_comment("", line, column, pos, _depth) do
    {"", line, column, pos}
  end

  defp consume_string("\"" <> rest, acc, line, column, pos) do
    {acc, rest, line, column + 1, pos + 1}
  end

  defp consume_string("\\" <> "\"" <> rest, acc, line, column, pos) do
    consume_string(rest, acc <> "\\\"", line, column + 2, pos + 2)
  end

  defp consume_string("\\" <> "n" <> rest, acc, line, column, pos) do
    consume_string(rest, acc <> "\\n", line, column + 2, pos + 2)
  end

  defp consume_string("\\" <> "t" <> rest, acc, line, column, pos) do
    consume_string(rest, acc <> "\\t", line, column + 2, pos + 2)
  end

  defp consume_string("\\" <> "r" <> rest, acc, line, column, pos) do
    consume_string(rest, acc <> "\\r", line, column + 2, pos + 2)
  end

  defp consume_string("\\" <> "\\" <> rest, acc, line, column, pos) do
    consume_string(rest, acc <> "\\\\", line, column + 2, pos + 2)
  end

  defp consume_string("\n" <> rest, acc, line, _column, pos) do
    consume_string(rest, acc <> "\n", line + 1, 1, pos + 1)
  end

  defp consume_string(<<c::utf8, rest::binary>>, acc, line, column, pos) do
    consume_string(rest, acc <> <<c::utf8>>, line, column + 1, pos + 1)
  end

  defp consume_string("", acc, line, column, pos) do
    {acc, "", line, column, pos}
  end

  # 1 regular char  →  'a'
  def consume_char(<<c::utf8, "'", rest::binary>>, line, column, pos)
      when c != ?\\ do
    {<<c::utf8>>, rest, line, column + 2, pos + 2}
  end

  # 2 escaped char  →  '\n'  '\t'  '\\'  '\''
  def consume_char(<<?\\, esc::utf8, "'", rest::binary>>, line, column, pos) do
    value =
      case esc do
        ?n -> "\n"
        ?t -> "\t"
        ?r -> "\r"
        ?\\ -> "\\"
        ?' -> "'"
        ?" -> "\""
        # fall-back: unknown escape kept verbatim
        _ -> <<esc::utf8>>
      end

    {value, rest, line, column + 3, pos + 3}
  end

  defp consume_number(<<c::utf8, rest::binary>>, acc, line, column, pos)
       when c >= ?0 and c <= ?9 do
    consume_number(rest, acc <> <<c::utf8>>, line, column + 1, pos + 1)
  end

  defp consume_number("." <> <<c::utf8, rest::binary>>, acc, line, column, pos)
       when c >= ?0 and c <= ?9 do
    consume_number(rest, acc <> "." <> <<c::utf8>>, line, column + 2, pos + 2)
  end

  defp consume_number(rest, acc, line, column, pos) do
    {acc, rest, column, pos}
  end

  defp consume_operator(input, line, column, pos) do
    # pick the first operator that matches the head of the input
    op = Enum.find(@operators, &String.starts_with?(input, &1))

    # fall back to the single leading character if nothing matched
    op = op || binary_part(input, 0, 1)

    op_size = byte_size(op)
    <<_::binary-size(op_size), rest::binary>> = input
    {op, rest, column + op_size, pos + op_size}
  end

  defp consume_identifier(<<c::utf8, rest::binary>>, acc, line, column, pos)
       when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or (c >= ?0 and c <= ?9) or c == ?_ do
    consume_identifier(rest, acc <> <<c::utf8>>, line, column + 1, pos + 1)
  end

  defp consume_identifier(rest, acc, line, column, pos) do
    {acc, rest, column, pos}
  end

  defp consume_hspace(<<" " <> rest>>, line, col, pos),
    do: consume_hspace(rest, line, col + 1, pos + 1)

  defp consume_hspace(<<"\t" <> rest>>, line, col, pos) do
    # Tab ⇒ advance to the next 8-column stop (feel free to pick 4 if you prefer)
    next = col + (8 - rem(col - 1, 8))
    consume_hspace(rest, line, next, pos + 1)
  end

  defp consume_hspace(rest, line, col, pos), do: {rest, line, col, pos}
end
