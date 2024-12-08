defmodule Puzzles.Day08 do
  def part1(input), do: run(input, :one)
  def part2(input), do: run(input, :two)

  def run(input, mode) do
    g = Grid.new_from_string(input)

    Grid.filter_fn(g, &(elem(&1, 1) != ".")).data
    |> Enum.group_by(&elem(&1, 1), &elem(&1, 0))
    |> Enum.flat_map(fn {_, points} ->
      Comb.pairs(points) |> Stream.flat_map(fn {a, b} -> antinodes(g, a, b, mode) end)
    end)
    |> Enum.uniq()
    |> Enum.count()
  end

  defp antinodes(grid, a, b, :one) do
    [add(a, subtract(a, b)), add(b, subtract(b, a))] |> Enum.reject(&Grid.is_oob(grid, &1))
  end

  defp antinodes(grid, a, b, :two) do
    {dx, dy} = subtract(a, b)
    gcd = Integer.gcd(dx, dy)
    {dx, dy} = {div(dx, gcd), div(dy, gcd)}

    Stream.concat(walk(grid, a, {dx, dy}), walk(grid, a, {-dx, -dy}))
  end

  defp walk(grid, point, dir) do
    Stream.unfold(point, &if(Grid.is_oob(grid, &1), do: nil, else: {&1, add(&1, dir)}))
  end

  defp subtract({x1, y1}, {x2, y2}), do: {x1 - x2, y1 - y2}

  defp add({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}
end
