defmodule Puzzles.Day03 do
  def part1(input), do: solve(input, false)
  def part2(input), do: solve(input, true)

  @spec solve(String.t(), boolean()) :: integer()
  def solve(input, expanded) do
    # Pluck all matches for `mul(X,Y)`, `don't` and `do`.
    ~r/(?:don't)|(?:do)|mul\((\d{1,3}),(\d{1,3})\)/
    |> Regex.scan(input)
    |> List.foldl({0, true}, fn
      ["do"], {acc, _} -> {acc, true}
      ["don't"], {acc, _} when not expanded -> {acc, true}
      ["don't"], {acc, _} when expanded -> {acc, false}
      _, {acc, false} -> {acc, false}
      [_, x, y], {acc, true} -> {acc + String.to_integer(x) * String.to_integer(y), true}
    end)
    |> elem(0)
  end
end
