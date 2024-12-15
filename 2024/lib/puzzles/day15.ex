defmodule Puzzles.Day15 do
  def part1(input), do: solve(input, & &1)

  def part2(input) do
    inflate = fn str ->
      str
      |> String.replace("#", "##")
      |> String.replace("O", "[]")
      |> String.replace(".", "..")
      |> String.replace("@", "@.")
    end

    solve(input, "[", inflate)
  end

  defp solve(input, char \\ "O", modifier) do
    {grid, instr} = parse(input, modifier)

    grid =
      Enum.reduce(instr, grid, fn instruction, acc ->
        struct!(grid, data: move(acc, instruction))
      end)

    Grid.filter_data(grid, [char]).data
    |> Enum.reduce(0, fn {{x, y}, _}, acc -> acc + (100 * y + x) end)
  end

  defp box_scan(map, points, {dx, dy} = vel, acc \\ []) do
    cond do
      Enum.any?(points, fn {x, y} -> Map.fetch!(map, {x + dx, y + dy}) == "#" end) ->
        # A wall is ahead of at least one of the points in the 'wave'. Bail.
        :error

      Enum.all?(points, fn {x, y} -> Map.fetch!(map, {x + dx, y + dy}) == "." end) ->
        # Empty space ahead of all points in 'wave'. We're done.
        {:ok, acc}

      true ->
        # Recurse while grabbing the new boxes we must be encountering.
        # These new boxes become the 'wavefront' of the search, and are the only
        # points considered when deciding when the search is done (i.e. reached a wall, or all empty space)
        new_points =
          Enum.reduce(points, [], fn {x, y}, acc ->
            n = Map.fetch!(map, {x + dx, y + dy})

            cond do
              Enum.any?(points, fn p -> p == {x + dx, y + dy} end) -> acc
              n == "[" -> [{x + dx, y + dy} | [{x + dx + 1, y + dy} | acc]]
              n == "]" -> [{x + dx - 1, y + dy} | [{x + dx, y + dy} | acc]]
              n == "O" -> [{x + dx, y + dy} | acc]
              n == "." -> acc
            end
          end)

        box_scan(map, new_points, vel, new_points ++ acc)
    end
  end

  defp move(grid, instr) do
    map = grid.data
    {{x, y} = pos, _} = Grid.find_value!(grid, "@")
    {dx, dy} = Grid.dir_to_delta(instr)
    {nx, ny} = {x + dx, y + dy}

    case Map.fetch(map, {nx, ny}) do
      {:ok, "#"} ->
        # Found a wall. No movement
        map

      {:ok, "."} ->
        # Empty space, robots position becomes empty, next position becomes robot
        map
        |> Map.replace(pos, ".")
        |> Map.replace({nx, ny}, "@")

      {:ok, "O"} ->
        shift_single_boxes(map, pos, {nx, ny}, {dx, dy})

      {:ok, "["} ->
        shift_joined_boxes(map, pos, {nx, ny}, {dx, dy})

      {:ok, "]"} ->
        shift_joined_boxes(map, pos, {nx, ny}, {dx, dy})
    end
  end

  defp shift_single_boxes(map, old, new, {dx, dy}) do
    case box_scan(map, [old], {dx, dy}) do
      :error ->
        map

      {:ok, [{ex, ey} | _]} ->
        map
        |> Map.replace(old, ".")
        |> Map.replace({ex + dx, ey + dy}, "O")
        |> Map.replace(new, "@")
    end
  end

  defp shift_joined_boxes(map, old, new, {dx, dy}) do
    case box_scan(map, [old], {dx, dy}) do
      :error ->
        map

      {:ok, boxes} ->
        Enum.chunk_every(boxes, 2)
        |> Enum.reduce(map, fn
          [{bsx, bsy} = bs, {bex, bey} = be], acc ->
            acc
            |> Map.replace(bs, ".")
            |> Map.replace(be, ".")
            |> Map.replace({bsx + dx, bsy + dy}, "[")
            |> Map.replace({bex + dx, bey + dy}, "]")

          _, acc ->
            acc
        end)
        |> Map.replace(old, ".")
        |> Map.replace(new, "@")
    end
  end

  defp parse(input, modifier) do
    [grid, dirs] = String.split(input, "\n\n", trim: true)
    grid = modifier.(grid)
    g = Grid.new_from_string(grid)

    dirs =
      String.replace(dirs, "\n", "")
      |> String.graphemes()
      |> Enum.map(fn
        "<" -> :left
        ">" -> :right
        "v" -> :down
        "^" -> :up
      end)

    {g, dirs}
  end
end
