defmodule Puzzles.Day02 do
  def part1(input) do
    parse_input(input) |> Enum.count(&is_safe(&1))
  end

  def part2(input) do
    parse_input(input) |> Enum.count(&is_safe_tolerant(&1))
  end

  defp is_safe(record) do
    diffs = get_diffs(record)

    Enum.all?(diffs, &(abs(&1) in 1..3)) &&
      (Enum.all?(diffs, &(&1 < 0)) || Enum.all?(diffs, &(&1 > 0)))
  end

  defp is_safe_tolerant(record) do
    is_safe(record) ||
      Enum.with_index(record)
      |> Enum.any?(fn {_, idx} -> is_safe(List.delete_at(record, idx)) end)
  end

  defp get_diffs(record) do
    Enum.chunk_every(record, 2, 1, :discard) |> Enum.map(fn [x, y] -> x - y end)
  end

  defp parse_input(input) do
    Input.parse_lines(input) |> Enum.map(&parse_record(&1))
  end

  defp parse_record(line) do
    String.split(line, " ") |> Enum.map(&String.to_integer/1)
  end
end
