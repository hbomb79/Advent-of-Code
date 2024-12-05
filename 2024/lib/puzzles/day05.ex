defmodule Puzzles.Day05 do
  def part1(input) do
    parse(input) |> elem(0) |> Enum.reduce(0, &(&2 + at_middle(&1)))
  end

  def part2(input) do
    parse(input) |> elem(1) |> Enum.reduce(0, &(&2 + at_middle(&1)))
  end

  defp at_middle(list), do: String.to_integer(Enum.at(list, floor(length(list) / 2)))

  defp sort_update(update, comes_before) do
    Enum.sort(update, fn a, b ->
      case {a in Map.get(comes_before, b, []), b in Map.get(comes_before, a, [])} do
        {true, false} -> true
        _ -> false
      end
    end)
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

    Input.parse_lines(updates)
    |> Enum.map(fn input -> String.split(input, ",", trim: true) end)
    |> split(comes_before)
  end

  defp split(updates, comes_before) do
    Enum.reduce(updates, {[], []}, fn upd, {valid, invalid} ->
      case sort_update(upd, comes_before) do
        ^upd -> {[upd | valid], invalid}
        diff -> {valid, [diff | invalid]}
      end
    end)
  end
end
