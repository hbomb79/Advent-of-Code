defmodule Puzzles.Day13 do
  def part1(input), do: solve(input, 0)
  def part2(input), do: solve(input, 10_000_000_000_000)

  defp solve(input, offset) do
    String.split(input, "\n", trim: true)
    |> Enum.chunk_every(3)
    |> Enum.reduce([], fn [buttonA, buttonB, prize], acc ->
      [{parse_xy(buttonA), parse_xy(buttonB), parse_xy(prize)} | acc]
    end)
    |> Enum.reduce(0, fn {{ax, ay}, {bx, by}, {prizex, prizey}}, acc ->
      prizex = prizex + offset
      prizey = prizey + offset

      # We're solving two linear equations, both with two unknowns (A and B):
      # ax * A + bx * B = prizex
      # ay * A + by * B = prizey
      #
      # After performing elimination on the equations, we can simplify to:
      d = ax * by - ay * bx
      a = (prizex * by - prizey * bx) / d
      b = (ax * prizey - ay * prizex) / d

      acc + if trunc(a) == a && trunc(b) == b, do: a * 3 + b, else: 0
    end)
  end

  defp parse_xy(str) do
    [_pre, xy] = String.split(str, ":")
    [[_match, x, y]] = Regex.scan(~r/X[\+\=](\d*), Y[\+\=](\d*)/, xy)
    {String.to_integer(x), String.to_integer(y)}
  end
end
