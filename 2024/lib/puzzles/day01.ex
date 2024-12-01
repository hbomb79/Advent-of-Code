defmodule Puzzles.Day01 do
  def part1(input) do
    {l, r} = parseInput(input)

    Enum.zip(l, r) |> Enum.map(fn {a, b} -> max(a, b) - min(a, b) end) |> Enum.sum()
  end

  def part2(input) do
    {l, r} = parseInput(input)
    rightFreqs = Enum.frequencies(r)

    Enum.map(l, &(&1 * Map.get(rightFreqs, &1, 0))) |> Enum.sum()
  end

  # Splits input strings on newline, and then on the consistent three whitespaces, before parsing
  # each entry to numbers and returning as the left/right lists.
  defp parseInput(input) do
    rows =
      String.split(input, "\n")
      |> Enum.filter(fn row -> String.length(row) > 0 end)
      |> Enum.map(fn row -> String.split(row, "   ") |> Enum.map(&String.to_integer/1) end)

    {
      Enum.map(rows, &List.first/1) |> Enum.sort(),
      Enum.map(rows, &List.last/1) |> Enum.sort()
    }
  end
end
