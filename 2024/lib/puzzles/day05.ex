defmodule Puzzles.Day05 do
  def part1(input) do
    {valid, _, _} = parse(input)

    Enum.reduce(valid, 0, fn upd, acc ->
      acc + String.to_integer(Enum.at(upd, floor(length(upd) / 2)))
    end)
  end

  def part2(input) do
    {_, invalid, comes_before} = parse(input)

    Enum.map(invalid, fn upd ->
      Enum.sort(upd, fn a, b ->
        case {a in Map.get(comes_before, b, []), b in Map.get(comes_before, a, [])} do
          {true, false} -> false
          _ -> true
        end
      end)
    end)
    |> Enum.reduce(0, fn upd, acc ->
      acc + String.to_integer(Enum.at(upd, floor(length(upd) / 2)))
    end)
  end

  defp is_correct([], _, _), do: true

  defp is_correct([hd | tl], must_come_before, seen) do
    case Map.fetch(must_come_before, hd) do
      :error ->
        # Nothing must come before this update
        is_correct(tl, must_come_before, MapSet.put(seen, hd))

      {:ok, vals} ->
        if Enum.all?(vals, &MapSet.member?(seen, &1)) do
          # If everything in vals is something we've seen already, all good
          is_correct(tl, must_come_before, MapSet.put(seen, hd))
        else
          # one or more items must come before this node
          false
        end
    end
  end

  defp parse(input) do
    [orders, updates] = String.split(input, "\n\n")

    comes_before =
      Input.parse_lines(orders)
      |> Enum.reduce(Map.new(), fn line, acc ->
        [bef, aft] = String.split(line, "|")

        case Map.fetch(acc, aft) do
          :error -> Map.put(acc, aft, [bef])
          {:ok, existing} -> Map.put(acc, aft, [bef | existing])
        end
      end)

    {valid, invalid} =
      Input.parse_lines(updates)
      |> Enum.map(fn input -> String.split(input, ",", trim: true) end)
      |> split(comes_before)

    {valid, invalid, comes_before}
  end

  defp split(updates, comes_before) do
    Enum.split_with(updates, fn upd ->
      deps = Map.new(comes_before, fn {k, v} -> {k, Enum.filter(v, fn vv -> vv in upd end)} end)
      is_correct(upd, deps, MapSet.new())
    end)
  end
end
