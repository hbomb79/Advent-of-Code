defmodule Puzzles.Day18 do
  def part1(input) do
    {grid, _rest_bytes} = parse(input)
    target = {grid.width - 1, grid.height - 1}

    Prioqueue.new([{0, {0, 0}}])
    |> dijkstra(%{}, %{}, target, neighbours(grid))
    |> Map.get(target)
  end

  def part2(input) do
    {grid, rest_bytes} = parse(input)
    find_fail(grid, rest_bytes, {0, 0}, {grid.width - 1, grid.height - 1})
  end

  defp find_fail(grid, [byte | rest], start, target) do
    grid = struct!(grid, data: Map.put(grid.data, byte, :corrupt))

    dists = Prioqueue.new([{0, start}]) |> dijkstra(%{}, %{}, target, neighbours(grid))

    case Map.get(dists, target) do
      nil -> byte
      _ -> find_fail(grid, rest, start, target)
    end
  end

  defp dijkstra(pq, dists, parents, target, nfn) do
    case Prioqueue.extract_min(pq) do
      {:ok, {{_cost, point} = node, pq}} ->
        {npq, ndists, nparents} =
          nfn.(node)
          |> Enum.reduce({pq, dists, parents}, fn {n_dist, n_point} = n, {pq, dists, parents} ->
            key = n_point

            cond do
              n_dist < Map.get(dists, key) ->
                {
                  Prioqueue.insert(pq, n),
                  Map.put(dists, key, n_dist),
                  Map.put(parents, key, MapSet.new([point]))
                }

              n_dist == Map.get(dists, key) ->
                {pq, dists, Map.update!(parents, key, &MapSet.put(&1, point))}

              true ->
                {pq, dists, parents}
            end
          end)

        dijkstra(npq, ndists, nparents, target, nfn)

      {:error, :empty} ->
        dists
    end
  end

  defp neighbours(g),
    do: fn {cost, current} ->
      Grid.neighbours(g, current)
      |> Enum.reject(fn p -> Grid.point!(g, p) == :corrupt end)
      |> Enum.map(fn p -> {cost + 1, p} end)
    end

  defp parse(input) do
    {bytes, width, height} =
      String.split(input, "\n", trim: true)
      |> Enum.reduce({[], 0, 0}, fn line, {acc, maxx, maxy} ->
        [x, y] = String.split(line, ",", trim: true) |> Enum.map(&String.to_integer/1)

        {[{x, y} | acc], max(maxx, x), max(maxy, y)}
      end)

    {bytes, rest_bytes} = Enum.reverse(bytes) |> Enum.split(1024)
    bytes = MapSet.new(bytes)

    state? = fn point -> if(MapSet.member?(bytes, point), do: :corrupt, else: :clear) end

    cells =
      for x <- 0..width, y <- 0..height, state = state?.({x, y}) do
        [data: state, x: x, y: y]
      end

    {Grid.new(cells), rest_bytes}
  end
end
