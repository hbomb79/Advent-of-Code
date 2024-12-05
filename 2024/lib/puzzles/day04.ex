defmodule Puzzles.Day04 do
  def part1(input) do
    grid = Grid.new_from_string(input)
    dirs = [:up, :down, :left, :right, :up_left, :up_right, :down_left, :down_right]

    Grid.filter_data(grid, ["X"]).data
    |> Map.keys()
    |> List.foldl(0, fn point, acc ->
      Enum.count(dirs, fn dir ->
        Grid.cast_ray(grid, point, dir, 3)
        |> Enum.map(&Grid.point!(grid, &1)) == ["X", "M", "A", "S"]
      end) + acc
    end)
  end

  def part2(input) do
    grid = Grid.new_from_string(input)

    Grid.filter_data(grid, ["A"]).data
    |> Map.keys()
    |> Enum.count(fn point ->
      points =
        [:up_left, :up_right, :down_left, :down_right]
        |> Enum.flat_map(
          &case Grid.cast_ray(grid, point, &1, 1) do
            [_, p] -> [p]
            _ -> []
          end
        )

      case points do
        [_, _, _, _] ->
          [topleft, topright, botleft, botright] = Enum.map(points, &Grid.point!(grid, &1))

          a = (topleft == "M" && botright == "S") || (topleft == "S" && botright == "M")
          b = (topright == "M" && botleft == "S") || (topright == "S" && botleft == "M")
          a && b

        _ ->
          false
      end
    end)
  end
end
