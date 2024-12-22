defmodule Puzzles.Day20 do
  def part1(input), do: run(input)
  def part2(input), do: run(input, 20)

  def run(input, max_radius \\ 2) do
    grid = Grid.new_from_string(input)

    {start, _} = Grid.find_value!(grid, "S")
    dists = walk_path(grid, start, %{start => 0})
    count_cheats(grid, dists, max_radius)
  end

  # This path has only ONE route (as per the puzzle statement), so we don't
  # need any search here, just walk it and capture the distances as we go
  defp walk_path(grid, current, dists) do
    case Grid.point!(grid, current) do
      "E" ->
        dists

      _ ->
        # Should only be one neighbour
        [n] =
          Grid.neighbours(grid, current)
          |> Enum.reject(fn p -> Map.has_key?(dists, p) || Grid.point!(grid, p) == "#" end)

        current_dist = Map.fetch!(dists, current)
        walk_path(grid, n, Map.put(dists, n, current_dist + 1))
    end
  end

  def count_cheats(grid, dists, radius_max) do
    pos = for x <- 0..(grid.width - 1), y <- 0..(grid.height - 1), do: {x, y}
    pos = Enum.reject(pos, fn p -> Grid.is_oob(grid, p) || Grid.point!(grid, p) == "#" end)

    Enum.flat_map(pos, fn {x, y} ->
      # Check each step along the radius from 2 to radius_max (inclusive) and see how many cheats
      # save us more than 100 picoseconds (accounting for the size of the cheat).
      Enum.flat_map(2..radius_max, fn radius ->
        Enum.map(0..radius, fn dx ->
          dy = radius - dx
          cdist = Map.fetch!(dists, {x, y})

          [{dx, dy}, {dx, -dy}, {-dx, dy}, {-dx, -dy}]
          |> Enum.map(fn {dx, dy} -> {x + dx, y + dy} end)
          |> Enum.uniq()
          |> Enum.reject(fn p -> Grid.is_oob(grid, p) || Grid.point!(grid, p) == "#" end)
          |> Enum.count(fn n -> cdist - Map.fetch!(dists, n) >= 100 + radius end)
        end)
      end)
    end)
    |> Enum.sum()
  end
end
