defmodule Puzzles.Day10 do
  def part1(input), do: trailscore(input, &trails_1/2)
  def part2(input), do: trailscore(input, &trails_2/2)

  defp trailscore(input, fun) do
    g = Grid.new_from_string(input) |> Grid.map(fn {k, v} -> {k, String.to_integer(v)} end)

    Enum.map(g.data, fn
      {point, 0} -> fun.(g, point) |> Enum.count()
      _ -> 0
    end)
    |> Enum.sum()
  end

  def trails_1(grid, start), do: bfs(grid, :queue.from_list([[start]]), MapSet.new([start]))

  def trails_2(grid, start) do
    start = [start]
    queue = :queue.from_list([{start, MapSet.new(start)}])
    bfs_all(grid, queue)
  end

  def bfs(grid, queue, visited, done \\ []) do
    case :queue.out(queue) do
      {{:value, [hd | _] = q}, queue} ->
        cond do
          Grid.point!(grid, hd) == 9 ->
            bfs(grid, queue, visited, [q | done])

          true ->
            {visited, queue} =
              neighbors(grid, hd, visited)
              |> Enum.reduce({visited, queue}, fn n, {vis, queue} ->
                {MapSet.put(vis, n), :queue.in([n | q], queue)}
              end)

            bfs(grid, queue, visited, done)
        end

      {:empty, _} ->
        done
    end
  end

  def bfs_all(grid, queue, done \\ []) do
    case :queue.out(queue) do
      {{:value, {[hd | _] = q, visited}}, queue} ->
        cond do
          Grid.point!(grid, hd) == 9 ->
            bfs_all(grid, queue, [q | done])

          true ->
            n = neighbors(grid, hd, visited)
            visited = Enum.reduce(n, visited, fn e, v -> MapSet.put(v, e) end)
            queue = Enum.reduce(n, queue, fn n, queue -> :queue.in({[n | q], visited}, queue) end)
            bfs_all(grid, queue, done)
        end

      {:empty, _} ->
        done
    end
  end

  defp neighbors(grid, point, visited) do
    threshold = Grid.point!(grid, point) + 1

    Grid.neighbours(grid, point)
    |> Enum.reject(fn candidate ->
      Grid.point!(grid, candidate) != threshold || MapSet.member?(visited, candidate)
    end)
  end
end
