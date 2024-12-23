defmodule Puzzles.Day23 do
  def part1(input), do: graph(input) |> find_connected_triplets()
  def part2(input), do: graph(input) |> largest_clique() |> Enum.join(",")

  defp find_connected_triplets(cns) do
    Enum.flat_map(cns, fn {from, to} ->
      Comb.pairs(MapSet.to_list(to))
      |> Enum.filter(fn {a, b} ->
        Map.fetch!(cns, a) |> MapSet.member?(b) && Map.fetch!(cns, b) |> MapSet.member?(a)
      end)
      |> Enum.map(fn {a, b} -> [from, a, b] |> Enum.sort() end)
    end)
    |> Enum.uniq()
    |> Enum.count(&Enum.any?(&1, fn a -> String.starts_with?(a, "t") end))
  end

  defp graph(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn pair -> String.split(pair, "-", trim: true) end)
    |> Enum.reduce(%{}, fn [a, b], acc ->
      acc
      |> Map.update(a, MapSet.new([b]), &MapSet.put(&1, b))
      |> Map.update(b, MapSet.new([a]), &MapSet.put(&1, a))
    end)
  end

  def largest_clique(graph) do
    {_, _, _, max} = find_cliques(graph, MapSet.new(), Map.keys(graph), MapSet.new())
    max
  end

  defp find_cliques(graph, cur, [], max) do
    if MapSet.size(cur) > MapSet.size(max),
      do: {graph, cur, [], cur},
      else: {graph, cur, [], max}
  end

  defp find_cliques(graph, cur, [v | rest] = rem, max) do
    if Enum.all?(cur, &MapSet.member?(Map.get(graph, v), &1)) do
      {_, _, _, max1} = find_cliques(graph, MapSet.put(cur, v), rest, max)
      {_, _, _, max2} = find_cliques(graph, cur, rest, max1)

      {graph, cur, rem, max2}
    else
      {_, _, _, max1} = find_cliques(graph, cur, rest, max)
      {graph, cur, rem, max1}
    end
  end
end
