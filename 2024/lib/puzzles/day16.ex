defmodule Puzzles.Day16 do
  def part1(input) do
    {seed, _parents, dists} = solve(input)
    Map.get(dists, seed)
  end

  def part2(input) do
    {seed, parents, _dists} = solve(input)
    walk_path([seed], parents, MapSet.new([seed])) |> MapSet.size()
  end

  defp solve(input) do
    g = Grid.new_from_string(input)
    {start, _} = Grid.find_value!(g, "S")
    {target, _} = Grid.find_value!(g, "E")

    {dists, parents} =
      Prioqueue.new([{0, start, :right}])
      |> dijkstra(%{}, %{}, target, neighbours(g))

    # Find cheapest direction for target
    dir =
      [:up, :down, :left, :right]
      |> Enum.map(&{Map.get(dists, {target, &1}), &1})
      |> Enum.min_by(&elem(&1, 0))
      |> elem(1)

    {{target, dir}, parents, dists}
  end

  defp neighbours(g) do
    fn {cost, current, dir} ->
      clockwise = Grid.rotate_dir(dir, :clockwise)
      aclockwise = Grid.rotate_dir(dir, :anticlockwise)

      [
        {cost + 1, Grid.shift!(g, dir, current), dir},
        {cost + 1001, Grid.shift!(g, clockwise, current), clockwise},
        {cost + 1001, Grid.shift!(g, aclockwise, current), aclockwise}
      ]
      |> Enum.reject(fn {_, p, _} -> Grid.point!(g, p) == "#" end)
    end
  end

  defp dijkstra(pq, dists, parents, target, nfn) do
    case Prioqueue.extract_min(pq) do
      {:ok, {{_cost, point, dir} = node, pq}} ->
        {npq, ndists, nparents} =
          nfn.(node)
          |> Enum.reduce({pq, dists, parents}, fn {n_dist, n_point, n_dir} = n,
                                                  {pq, dists, parents} ->
            key = {n_point, n_dir}

            cond do
              n_dist < Map.get(dists, key) ->
                {
                  Prioqueue.insert(pq, n),
                  Map.put(dists, key, n_dist),
                  Map.put(parents, key, MapSet.new([{point, dir}]))
                }

              n_dist == Map.get(dists, key) ->
                {pq, dists, Map.update!(parents, key, &MapSet.put(&1, {point, dir}))}

              true ->
                {pq, dists, parents}
            end
          end)

        dijkstra(npq, ndists, nparents, target, nfn)

      {:error, :empty} ->
        {dists, parents}
    end
  end

  defp walk_path([], _, tiles), do: tiles

  defp walk_path([hd | rest], parents, tiles) do
    {queue, tiles} =
      Map.get(parents, hd, [])
      |> Enum.reduce({rest, tiles}, fn {tile, _} = parent, {q, ts} ->
        {[parent | q], MapSet.put(ts, tile)}
      end)

    walk_path(queue, parents, tiles)
  end
end
