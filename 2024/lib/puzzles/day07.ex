defmodule Puzzles.Day07 do
  def part1(input), do: run(input, [:add, :mul])
  def part2(input), do: run(input, [:add, :mul, :concat])

  def run(input, ops) do
    Input.parse_lines(input)
    |> Stream.map(fn line ->
      [desired, parts] = String.split(line, ": ", trim: true)
      {String.to_integer(desired), String.split(parts, " ") |> Enum.map(&String.to_integer/1)}
    end)
    |> Stream.filter(fn {desired, [p | ps]} ->
      solve([p], ps, ops) |> Enum.any?(&(&1 == desired))
    end)
    |> Stream.map(fn {desired, _} -> desired end)
    |> Enum.sum()
  end

  def solve(acc, [], _), do: acc

  def solve(acc, [x | xs], ops) do
    Enum.flat_map(ops, fn op ->
      Enum.map(acc, fn acc ->
        case op do
          :add -> acc + x
          :mul -> acc * x
          :concat -> String.to_integer("#{acc}#{x}")
        end
      end)
    end)
    |> solve(xs, ops)
  end
end
