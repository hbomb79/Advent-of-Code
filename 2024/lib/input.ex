defmodule Input do
  @spec parse_lines(String.t()) :: [String.t()]
  def parse_lines(input) do
    String.split(input, "\n", trim: true)
  end
end
