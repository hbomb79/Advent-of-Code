defmodule Puzzles.Day12 do
  def part1(input) do
    g = Grid.new_from_string(input)

    regions(g)
    |> Enum.reduce(0, fn {region, _type}, acc ->
      acc + MapSet.size(region) * perimeter(region)
    end)
  end

  def part2(input) do
    g = Grid.new_from_string(input)

    regions(g)
    |> Enum.reduce(0, fn {region, type}, acc ->
      acc + MapSet.size(region) * corners(g, region, type)
    end)
  end

  defp perimeter(region) do
    Enum.map(region, fn {x, y} ->
      [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
      |> Enum.count(&(!MapSet.member?(region, &1)))
    end)
    |> Enum.sum()
  end

  defp corners(grid, region, type) do
    Enum.reduce(region, 0, fn {x, y}, acc ->
      n =
        [:left, :right, :up, :down]
        |> Enum.map(&{&1, Grid.rotate_dir(&1, :clockwise)})
        |> Enum.map(fn {a, b} ->
          {adx, ady} = Grid.dir_to_delta(a)
          {bdx, bdy} = Grid.dir_to_delta(b)

          [
            {x + adx, y + ady},
            {x + bdx, y + bdy},
            {x + adx + bdx, y + ady + bdy}
          ]
          |> Enum.map(fn point ->
            if Grid.is_oob(grid, point), do: nil, else: Grid.point!(grid, point)
          end)
        end)
        |> Enum.count(fn [left, right, middle] ->
          (left != type && right != type) || (left == type && right == type && middle != type)
        end)

      acc + n
    end)
  end

  defp regions(g) do
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
  end

  defp floodfill(grid, point, type, seen \\ MapSet.new()) do
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
      seen
    else
      Enum.reduce(n, seen, fn point, seen -> floodfill(grid, point, type, seen) end)
    end
  end
end
