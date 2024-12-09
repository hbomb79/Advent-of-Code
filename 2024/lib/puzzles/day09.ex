defmodule Puzzles.Day09 do
  def part1(input) do
    parts = parse(input, :singles) |> elem(0) |> Enum.reverse()

    swap(parts, length(parts) - 1, [], [])
    |> Stream.with_index()
    |> Stream.reject(fn {e, _} -> e[:kind] == :empty end)
    |> Enum.reduce(0, fn {e, idx}, acc -> acc + Keyword.get(e, :id) * idx end)
  end

  def part2(input) do
    {list, max_id, _} = parse(input, :batch)

    bulk_swap(Enum.reverse(list), max_id - 1)
    |> merge()
    |> Stream.flat_map(&List.duplicate(&1, &1[:size]))
    |> Stream.with_index()
    |> Stream.reject(fn {e, _} -> e[:kind] == :empty end)
    |> Enum.reduce(0, fn {e, idx}, acc -> acc + e[:id] * idx end)
  end

  defp swap(_, -1, pre, post), do: Enum.reverse(pre) ++ Enum.reverse(post)

  defp swap([empty | tl] = list, file_idx, pre, post) do
    last = Enum.at(list, file_idx)

    cond do
      last[:kind] == :empty ->
        swap(list, file_idx - 1, pre, [last | post])

      empty[:kind] == :file ->
        swap(tl, file_idx - 1, [empty | pre], post)

      empty[:kind] == :empty ->
        swap(tl, file_idx - 2, [last | pre], [empty | post])
    end
  end

  defp bulk_swap(list, -1), do: list

  defp bulk_swap(list, id) do
    to_move_idx = Enum.find_index(list, fn e -> e[:kind] == :file && e[:id] == id end)
    to_move = Enum.at(list, to_move_idx)

    # Find first element with adequate space
    empty_idx =
      Enum.find_index(list, fn e -> e[:kind] == :empty && e[:size] >= to_move[:size] end)

    if empty_idx == nil || empty_idx > to_move_idx do
      bulk_swap(list, id - 1)
    else
      empty = Enum.at(list, empty_idx)

      new_list =
        if empty[:size] > to_move[:size] do
          list
          |> List.delete_at(to_move_idx)
          |> List.insert_at(to_move_idx, [{:kind, :empty}, {:size, to_move[:size]}])
          |> List.delete_at(empty_idx)
          |> List.insert_at(empty_idx, to_move)
          |> List.insert_at(empty_idx + 1, [
            {:kind, :empty},
            {:size, empty[:size] - to_move[:size]}
          ])
        else
          list
          |> List.delete_at(to_move_idx)
          |> List.delete_at(empty_idx)
          |> List.insert_at(empty_idx, to_move)
          |> List.insert_at(to_move_idx, empty)
        end

      bulk_swap(new_list, id - 1)
    end
  end

  def merge([hd | tl]) do
    Enum.reduce(tl, [hd], fn element, [a | rest] = acc ->
      kind = element[:kind]
      size = element[:size]

      if kind == :empty && a[:kind] == :empty do
        [[{:kind, :empty}, {:size, size + a[:size]}] | rest]
      else
        [element | acc]
      end
    end)
    |> Enum.reverse()
  end

  defp parse(input, mode) do
    String.trim(input)
    |> String.graphemes()
    |> Enum.reduce({[], 0, true}, fn digit, {acc, idx, is_file} ->
      kind = if is_file, do: :file, else: :empty
      digit = String.to_integer(digit)

      n = [{:kind, kind}]
      n = if is_file, do: Keyword.put(n, :id, idx), else: n

      idx = if is_file, do: idx + 1, else: idx

      case mode do
        :singles -> {List.duplicate(n, digit) ++ acc, idx, !is_file}
        :batch -> {[Keyword.put(n, :size, digit) | acc], idx, !is_file}
      end
    end)
  end
end
