defmodule ImplementationParser do
  import SweetXml

  @doc """
  Parses an XML string representing an implementation into an Elixir map.

  ## Examples

      iex> xml_string = \"\"\"
      ...> <implementation>
      ...>  <name>My Awesome Algorithm</name>
      ...>  <code>
      ...> defmodule MyAlgorithm do
      ...>   def run(data) do
      ...>     # complex logic here
      ...>     IO.inspect(data, label: "Running with")
      ...>   end
      ...> end
      ...>  </code>
      ...>  <tests>
      ...>   <test>assert MyAlgorithm.run([1,2,3]) == :ok</test>
      ...>   <test>assert MyAlgorithm.run([]) == :ok</test>
      ...>  </tests>
      ...> </implementation>
      ...> \"\"\"
      iex> ImplementationParser.parse(xml_string)
      %{
        name: "My Awesome Algorithm",
        code: "\\ndefmodule MyAlgorithm do\\n  def run(data) do\\n    # complex logic here\\n    IO.inspect(data, label: \\"Running with\\")\\n  end\\nend\\n ",
        tests: [
          "assert MyAlgorithm.run([1,2,3]) == :ok",
          "assert MyAlgorithm.run([]) == :ok"
        ]
      }

      iex> xml_string_cdata = \"\"\"
      ...> <implementation>
      ...>  <name>Algorithm with CDATA</name>
      ...>  <code><![CDATA[
      ...> defmodule MyCDATAlgorithm do
      ...>   # Code with < & > symbols
      ...>   def check(a, b) when a < b, do: :less_than
      ...>   def check(_, _), do: :other
      ...> end
      ...> ]]></code>
      ...>  <tests>
      ...>   <test><![CDATA[assert MyCDATAlgorithm.check(1, 2) == :less_than]]></test>
      ...>  </tests>
      ...> </implementation>
      ...> \"\"\"
      iex> ImplementationParser.parse(xml_string_cdata)
      %{
        name: "Algorithm with CDATA",
        code: "\\ndefmodule MyCDATAlgorithm do\\n  # Code with < & > symbols\\n  def check(a, b) when a < b, do: :less_than\\n  def check(_, _), do: :other\\nend\\n",
        tests: ["assert MyCDATAlgorithm.check(1, 2) == :less_than"]
      }

  """
  def parse(xml_string) do
    xml_string
    # Parses the string into an internal XML document representation
    |> SweetXml.parse()
    |> SweetXml.xpath(
      # The ~x sigil is for XPath expressions.
      # The 'o' at the end of the first XPath means "object" or "one map".
      # We expect one <implementation> tag at the root or anywhere.
      # If you know it's always the root, you could use ~x"/implementation"o
      ~x"//implementation"o,
      # 's' means string. Gets text content of <name>
      name: ~x"./name/text()"s,
      # For the code block, we might want to trim whitespace if it's not CDATA.
      # If it's CDATA, text() will preserve it correctly.
      # If it's plain text with newlines and indentation, text() includes those.
      code: ~x"./code/text()"s |> transform_by(&String.trim/1),
      # 'ls' means list of strings. Gets text content of each <test> tag.
      tests: ~x"./tests/test/text()"ls
    )
  end

  @doc """
  Parses an XML file containing an implementation.
  """
  def parse_file(file_path) do
    file_path
    # Parses the file
    |> SweetXml.parse_file()
    |> SweetXml.xpath(
      ~x"//implementation"o,
      name: ~x"./name/text()"s,
      code: ~x"./code/text()"s |> transform_by(&String.trim/1),
      tests: ~x"./tests/test/text()"ls
    )
  end
end
