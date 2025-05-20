defmodule Prompts.ImplementationXmlParser do
  @doc """
  Parses a custom XML format containing <implementation>, <name>, <code>, and <tests> sections.
  Handles content that might contain '>' symbols without treating them as XML markers.
  """
  def parse(xml_string) do
    # Extract the main implementation block
    %{content: implementation} = extract_tag(xml_string, "implementation")

    # Extract the nested sections from the implementation
    %{content: name} = extract_tag(implementation, "name")
    %{content: code} = extract_tag(implementation, "code")

    # Extract and parse tests
    %{content: tests_content} = extract_tag(implementation, "tests")
    tests = parse_tests(tests_content)

    # Return structured data
    %{
      name: String.trim(name),
      code: String.trim(code),
      tests: tests
    }
  end

  @doc """
  Extracts content between matching opening and closing tags.
  Handles nested tags by counting opening and closing tag occurrences.
  """
  def extract_tag(content, tag_name) do
    opening_tag = "<#{tag_name}>"
    closing_tag = "</#{tag_name}>"

    case :binary.match(content, opening_tag) do
      {start_pos, tag_len} ->
        start_content = start_pos + tag_len

        # Find the matching closing tag by counting tag levels
        {end_pos, content_len} =
          find_matching_closing_tag(
            binary_part(content, start_content, byte_size(content) - start_content),
            tag_name
          )

        extracted_content = binary_part(content, start_content, end_pos)

        # Calculate positions correctly - fix the arithmetic error
        closing_tag_size = byte_size(closing_tag)
        next_pos = start_content + end_pos + closing_tag_size

        remainder =
          if next_pos < byte_size(content) do
            binary_part(content, next_pos, byte_size(content) - next_pos)
          else
            ""
          end

        %{
          content: extracted_content,
          remainder: remainder
        }

      :nomatch ->
        %{content: "", remainder: content}
    end
  end

  @doc """
  Finds the position of the matching closing tag, accounting for nested tags.
  """
  def find_matching_closing_tag(content, tag_name) do
    opening_tag = "<#{tag_name}>"
    closing_tag = "</#{tag_name}>"

    find_matching_closing_tag_impl(content, tag_name, 0, 0)
  end

  defp find_matching_closing_tag_impl(content, tag_name, pos, level) do
    opening_tag = "<#{tag_name}>"
    closing_tag = "</#{tag_name}>"

    opening_match = :binary.match(content, opening_tag, scope: {pos, byte_size(content) - pos})
    closing_match = :binary.match(content, closing_tag, scope: {pos, byte_size(content) - pos})

    cond do
      # No more tags found, but we're looking for one
      closing_match == :nomatch ->
        {byte_size(content), 0}

      # Found an opening tag first
      opening_match != :nomatch and
          (closing_match == :nomatch or elem(opening_match, 0) < elem(closing_match, 0)) ->
        {opening_pos, _} = opening_match

        find_matching_closing_tag_impl(
          content,
          tag_name,
          opening_pos + byte_size(opening_tag),
          level + 1
        )

      # Found a closing tag and it's the matching one for our current level
      level == 0 ->
        {closing_pos, _} = closing_match
        {closing_pos, closing_pos}

      # Found a closing tag, but we need to continue because of nesting
      true ->
        {closing_pos, _} = closing_match

        find_matching_closing_tag_impl(
          content,
          tag_name,
          closing_pos + byte_size(closing_tag),
          level - 1
        )
    end
  end

  @doc """
  Parses the tests section into a list of test definitions.
  """
  def parse_tests(tests_content) do
    extract_all_tags(tests_content, "test")
    |> Enum.map(&String.trim/1)
  end

  @doc """
  Extracts all occurrences of a specific tag from content.
  """
  def extract_all_tags(content, tag_name) do
    extract_all_tags_impl(content, tag_name, [])
  end

  defp extract_all_tags_impl(content, tag_name, acc) do
    case extract_tag(content, tag_name) do
      %{content: "", remainder: _} ->
        Enum.reverse(acc)

      %{content: extracted, remainder: remainder} ->
        extract_all_tags_impl(remainder, tag_name, [extracted | acc])
    end
  end
end
