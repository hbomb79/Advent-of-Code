defmodule Puzzles.Day19 do
  def part1(input) do
    {patterns, towels} = parse(input)
    Enum.count(towels, &(get_combinations(&1, patterns) != 0))
  end

  def part2(input) do
    {patterns, towels} = parse(input)
    Enum.reduce(towels, 0, &(&2 + get_combinations(&1, patterns)))
  end

  defp get_combinations(towel, patterns), do: combinations(towel, patterns, %{}) |> elem(0)

  defp combinations("", _, cache), do: {1, cache}

  defp combinations(towel, patterns, cache) do
    case Map.fetch(cache, towel) do
      :error ->
        Enum.filter(patterns, fn patt -> String.starts_with?(towel, patt) end)
        |> Enum.reduce({0, cache}, fn patt, {acc, cache} ->
          {_, towel} = String.split_at(towel, String.length(patt))
          {res, new_cache} = combinations(towel, patterns, cache)

          {acc + res, Map.put(new_cache, towel, res)}
        end)

      {:ok, hit} ->
        {hit, cache}
    end
  end

  defp parse(input) do
    [patterns, towels] = String.split(input, "\n\n")
    {String.split(patterns, ", ", trim: true), String.split(towels, "\n", trim: true)}
  end
end
