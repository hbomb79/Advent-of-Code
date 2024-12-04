defmodule Puzzles.Day04 do
  def part1(input) do
    grid = Grid.new_from_string(input)

    Grid.filter_data(grid, ["X"]).data
    |> Map.keys()
    |> Enum.map(fn point ->
      Enum.count(
        [:up, :down, :left, :right, :up_left, :up_right, :down_left, :down_right],
        fn dir ->
          case Grid.cast_ray(grid, point, dir, 3) do
            ps when length(ps) == 4 ->
              Enum.map(ps, &Grid.point!(grid, &1)) == ["X", "M", "A", "S"]

            _ ->
              false
          end
        end
      )
    end)
    |> Enum.sum()
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
        [tl, tr, bl, br] ->
          topleft = Grid.point!(grid, tl)
          topright = Grid.point!(grid, tr)
          botleft = Grid.point!(grid, bl)
          botright = Grid.point!(grid, br)

          a = (topleft == "M" && botright == "S") || (topleft == "S" && botright == "M")
          b = (topright == "M" && botleft == "S") || (topright == "S" && botleft == "M")
          a && b

        _ ->
          false
      end
    end)
  end
end
