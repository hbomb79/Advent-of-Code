defmodule Input do
  def parse_lines(input) do
    String.split(input, "\n") |> Enum.filter(&(String.length(&1) > 0))
  end
end
