defmodule Puzzles.Day16 do
  def part1(input) do
    {seed, _parents, dists} = solve(input)
    Map.get(dists, seed)
  end

  def part2(input) do
    {seed, parents, _dists} = solve(input)
    path_tiles([seed], parents, MapSet.new([seed])) |> MapSet.size()
  end

  defp solve(input) do
    g = Grid.new_from_string(input)
    {start, _} = Grid.find_value!(g, "S")
    {target, _} = Grid.find_value!(g, "E")

    {dists, parents} =
      djikstra(Prioqueue.new([{0, start, :right}]), Map.new(), Map.new(), target, neighbours(g))

    # Find cheapest direction for target
    dir =
      [:up, :down, :left, :right]
      |> Enum.map(&{Map.get(dists, {target, &1}), &1})
      |> Enum.min_by(&elem(&1, 0))
      |> elem(1)

    {{target, dir}, parents, dists}
  end

  defp neighbours(g) do
    fn {current, cost, dir} ->
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

  defp path_tiles([], _, tiles), do: tiles

  defp path_tiles([hd | rest], parents, tiles) do
    ps = Map.get(parents, hd, [])

    {queue, tiles} =
      Enum.reduce(ps, {rest, tiles}, fn {tile, _} = parent, {q, ts} ->
        {[parent | q], MapSet.put(ts, tile)}
      end)

    path_tiles(queue, parents, tiles)
  end

  defp djikstra(pq, dists, parents, target, nfn) do
    case Prioqueue.extract_min(pq) do
      {:ok, {{dist, point, dir}, pq}} ->
        {npq, ndists, nparents} =
          nfn.({point, dist, dir})
          |> Enum.reduce({pq, dists, parents}, fn n, {pq, dists, parents} ->
            {n_dist, n_point, n_dir} = n
            key = {n_point, n_dir}

            cond do
              n_dist < Map.get(dists, key) ->
                {
                  Prioqueue.insert(pq, n),
                  Map.put(dists, key, n_dist),
                  Map.put(parents, key, MapSet.new([{point, dir}]))
                }

              n_dist == Map.get(dists, key) ->
                {pq, dists,
                 Map.update!(parents, key, fn existing -> MapSet.put(existing, {point, dir}) end)}

              true ->
                {pq, dists, parents}
            end
          end)

        djikstra(npq, ndists, nparents, target, nfn)

      {:error, :empty} ->
        {dists, parents}
    end
  end
end
