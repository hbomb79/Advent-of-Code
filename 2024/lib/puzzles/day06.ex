defmodule Puzzles.Day06 do
  def part1(input) do
    grid = input |> Grid.new_from_string()
    {start, _} = Grid.find_value!(grid, "^")

    solve(grid, start, :up, MapSet.new(), div(map_size(grid.data), 2) + 100)
    |> elem(1)
    |> MapSet.size()
  end

  def part2(input) do
    grid = input |> Grid.new_from_string()

    steplimit = div(map_size(grid.data), 2) + 100
    {start, _} = Grid.find_value!(grid, "^")

    solve(grid, start, :up, MapSet.new(), 1_000_000_000)
    |> elem(1)
    |> Enum.count(fn obs ->
      ngrid = struct(grid, data: Map.put(grid.data, obs, "#"))

      case solve(ngrid, start, :up, MapSet.new(), steplimit) do
        {false, _} -> true
        _ -> false
      end
    end)
  end

  defp solve(_, _, _, _, 0), do: {false, nil}

  defp solve(grid, current, dir, touched, steps_remaining) do
    touched = MapSet.put(touched, current)

    case Grid.shift(grid, dir, current) do
      {:ok, point} ->
        rem = steps_remaining - 1

        case Grid.point!(grid, point) do
          "#" -> solve(grid, current, Grid.rotate_dir(dir, :clockwise), touched, rem)
          _ -> solve(grid, point, dir, MapSet.put(touched, point), rem)
        end

      {:error, _} ->
        {true, touched}
    end
  end
end
