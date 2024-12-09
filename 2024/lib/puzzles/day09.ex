defmodule Puzzles.Day09 do
  def part1(input) do
    String.trim(input)
    |> String.graphemes()
    |> Enum.reduce({[], 0, true}, fn digit, {acc, idx, is_file} ->
      kind = if is_file, do: :file, else: :empty
      n = [{:kind, kind}]
      n = if is_file, do: Keyword.put(n, :id, idx), else: n

      n = List.duplicate(n, String.to_integer(digit))

      idx = if is_file, do: idx + 1, else: idx
      {n ++ acc, idx, !is_file}
    end)
    |> elem(0)
    |> Enum.reverse()
    |> swap()
    |> Enum.with_index()
    |> Enum.reduce(0, fn {e, idx}, acc ->
      if Keyword.get(e, :kind) == :file do
        acc + Keyword.get(e, :id) * idx
      else
        acc
      end
    end)
  end

  def part2(input) do
    {list, max_id, _} =
      String.trim(input)
      |> String.graphemes()
      |> Enum.reduce({[], 0, true}, fn digit, {acc, idx, is_file} ->
        kind = if is_file, do: :file, else: :empty
        n = [{:kind, kind}, {:size, String.to_integer(digit)}]
        n = if is_file, do: Keyword.put(n, :id, idx), else: n

        idx = if is_file, do: idx + 1, else: idx
        {[n | acc], idx, !is_file}
      end)

    bulk_swap(Enum.reverse(list), max_id - 1)
    |> merge()
    |> Enum.flat_map(fn e ->
      List.duplicate(e, e[:size])
    end)
    |> Enum.with_index()
    |> Enum.reduce(0, fn {e, idx}, acc ->
      if Keyword.get(e, :kind) == :file do
        acc + e[:id] * idx
      else
        acc
      end
    end)
  end

  defp swap(list) do
    # Find last file element
    rev = Enum.reverse(list)
    last_idx = rev |> Enum.find_index(fn e -> e[:kind] == :file end)
    last = Enum.at(rev, last_idx)

    # Find first element with space
    empty_idx = Enum.find_index(list, fn e -> e[:kind] == :empty end)
    empty = Enum.at(list, empty_idx)

    last_idx = length(list) - last_idx - 1

    # If empty idx is to the right of the last, then we must be done
    if empty_idx >= last_idx do
      list
    else
      new_list =
        list
        |> List.delete_at(last_idx)
        |> List.delete_at(empty_idx)
        |> List.insert_at(empty_idx, last)
        |> List.insert_at(last_idx, empty)

      swap(new_list)
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
end
