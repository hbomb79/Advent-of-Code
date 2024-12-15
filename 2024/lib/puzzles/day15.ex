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
      Enum.any?(points, &(Map.fetch!(map, add(&1, vel)) == "#")) ->
        # A wall is ahead of at least one of the points in the 'wave'. Bail.
        :error

      Enum.all?(points, &(Map.fetch!(map, add(&1, vel)) == ".")) ->
        # Empty space ahead of all points in 'wave'. We're done.
        {:ok, acc}

      true ->
        # Grab the new obstacles ahead of us and recurse. These new boxes become the 'wavefront' of the search.
        new_points =
          points
          |> Enum.reject(&Enum.member?(points, add(&1, vel)))
          |> Enum.reduce([], fn {x, y} = p, acc ->
            case Map.fetch!(map, add(p, vel)) do
              "[" -> [{x + dx, y + dy} | [{x + dx + 1, y + dy} | acc]]
              "]" -> [{x + dx - 1, y + dy} | [{x + dx, y + dy} | acc]]
              "O" -> [{x + dx, y + dy} | acc]
              "." -> acc
            end
          end)

        box_scan(map, new_points, vel, new_points ++ acc)
    end
  end

  defp move(grid, instr) do
    {pos, _} = Grid.find_value!(grid, "@")
    delta = Grid.dir_to_delta(instr)
    npos = add(pos, delta)
    map = grid.data

    case Map.fetch!(map, npos) do
      "#" -> map
      "." -> %{map | pos => ".", npos => "@"}
      "O" -> shift_single_boxes(map, pos, npos, delta)
      "[" -> shift_joined_boxes(map, pos, npos, delta)
      "]" -> shift_joined_boxes(map, pos, npos, delta)
    end
  end

  defp shift_single_boxes(map, old, new, delta) do
    case box_scan(map, [old], delta) do
      :error -> map
      {:ok, [empty | _]} -> %{map | old => ".", add(empty, delta) => "O", new => "@"}
    end
  end

  defp add({px, py}, {dx, dy}), do: {px + dx, py + dy}

  defp shift_joined_boxes(map, old, new, delta) do
    case box_scan(map, [old], delta) do
      :error ->
        map

      {:ok, boxes} ->
        Enum.chunk_every(boxes, 2)
        |> Enum.reduce(map, fn [bs, be], acc ->
          bs_new = add(bs, delta)
          be_new = add(be, delta)
          %{acc | bs => ".", be => ".", bs_new => "[", be_new => "]"}
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
