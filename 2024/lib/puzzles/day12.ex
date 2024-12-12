defmodule Puzzles.Day12 do
  def part1(input) do
    g = Grid.new_from_string(input)

    Stream.unfold(g, fn g ->
      if map_size(g.data) == 0 do
        nil
      else
        [{seed, type}] = Enum.take(g.data, 1)
        region = floodfill(g, seed, type)

        new_data = Grid.filter_fn(g, fn {point, _} -> !MapSet.member?(region, point) end)

        {{region, type}, new_data}
      end
    end)
    |> Enum.to_list()
    |> Enum.map(fn {region, _type} ->
      MapSet.size(region) * perimeter(region)
    end)
    |> Enum.sum()
  end

  defp perimeter(region) do
    Enum.map(region, fn {x, y} ->
      [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
      |> Enum.count(&(!MapSet.member?(region, &1)))
    end)
    |> Enum.sum()
  end

  def part2(_input) do
  end

  defp floodfill(grid, point, type, seen \\ MapSet.new()) do
    # Recursively spread out from the point, until all connecting grid points
    # of the same type have been found.

    seen = MapSet.put(seen, point)

    n =
      Grid.neighbours(grid, point)
      |> Enum.reject(fn point ->
        case Grid.point(grid, point) do
          :error -> true
          {:ok, v} -> v != type
        end
      end)
      |> Enum.reject(&MapSet.member?(seen, &1))

    if length(n) == 0 do
      # No more neighbours, done
      seen
    else
      Enum.reduce(n, seen, fn point, seen -> floodfill(grid, point, type, seen) end)
    end
  end
end
