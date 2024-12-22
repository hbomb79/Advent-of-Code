defmodule Puzzles.Day21 do
  use Memoize

  def part1(input), do: run(input, 2)
  def part2(input), do: run(input, 25)

  def run(input, num) do
    np = get_keypad_grid(:keypad)
    np_costs = calc_costs(np)

    dp = get_keypad_grid(:directional)
    dp_costs = calc_costs(dp)

    String.split(input, "\n", trim: true)
    |> Enum.reduce(0, fn line, acc ->
      n = String.trim_trailing(line, "A") |> String.to_integer()

      min =
        solve(line, np_costs)
        |> Enum.map(&Enum.join/1)
        |> Enum.map(fn inp -> calc_len(inp, num, dp_costs) end)
        |> Enum.min()

      acc + n * min
    end)
  end

  def solve(target, costs) do
    Enum.map(zip_target(target), &Map.fetch!(costs, &1)) |> cross()
  end

  def calc_costs(keypad) do
    valid = keypad.data |> Map.filter(fn {_pos, v} -> v != "#" end) |> Map.keys()
    valid = for x <- valid, y <- valid, do: {x, y}

    Enum.reduce(valid, %{}, fn
      {x, x}, acc ->
        # Same point, only one path here: "A"
        val = Grid.point!(keypad, x)
        Map.put(acc, {val, val}, ["A"])

      {x, y}, acc ->
        # BFS from the two points to find the possibile paths x -> y
        Map.put(
          acc,
          {Grid.point!(keypad, x), Grid.point!(keypad, y)},
          search(keypad, :queue.from_list([{x, ""}]), y)
        )
    end)
  end

  def search(keypad, queue, target, optimal \\ :infinity, poss \\ []) do
    case :queue.out(queue) do
      {{:value, {current, path}}, queue} ->
        ns =
          [{:up, "^"}, {:left, "<"}, {:right, ">"}, {:down, "v"}]
          |> Enum.map(fn {dir, symbol} -> {symbol, Grid.shift(keypad, dir, current)} end)
          |> List.foldl([], fn
            {_dir, {:error, _}}, acc -> acc
            {dir, {:ok, point}}, acc -> [{point, dir} | acc]
          end)
          |> Enum.reject(fn {p, _dir} -> Grid.point!(keypad, p) == "#" end)

        case process_neighbours(ns, path, target, queue, poss, optimal) do
          {:cont, queue, poss, optimal} -> search(keypad, queue, target, optimal, poss)
          {:halt, poss} -> poss
        end

      {:empty, _queue} ->
        poss
    end
  end

  defp process_neighbours([], _path, _target, queue, poss, optimal),
    do: {:cont, queue, poss, optimal}

  defp process_neighbours([{n, symbol} | ns], path, target, queue, poss, optimal) do
    cond do
      n == target && optimal < String.length(path) + 1 ->
        # This path is longer than the optimal. This is a BFS, which means all paths
        # from here on out will be longer, so we can stop.
        {:halt, poss}

      n == target ->
        poss = [path <> symbol <> "A" | poss]
        optimal = String.length(path) + 1
        process_neighbours(ns, path, target, queue, poss, optimal)

      true ->
        process_neighbours(ns, path, target, :queue.in({n, path <> symbol}, queue), poss, optimal)
    end
  end

  defmemo calc_len(target, 1, dp_costs) do
    zip_target(target)
    |> Enum.map(fn p -> Map.fetch!(dp_costs, p) |> Enum.at(0) |> String.length() end)
    |> Enum.sum()
  end

  defmemo calc_len(target, depth, dp_costs) do
    zip_target(target)
    |> Enum.map(fn {a, b} ->
      Map.fetch!(dp_costs, {a, b})
      |> Enum.map(fn path -> calc_len(path, depth - 1, dp_costs) end)
      |> Enum.min()
    end)
    |> Enum.sum()
  end

  def zip_target(target), do: Enum.zip(String.graphemes("A" <> target), String.graphemes(target))
  def get_keypad_grid(:keypad), do: "789\n456\n123\n#0A" |> Grid.new_from_string()
  def get_keypad_grid(:directional), do: "#^A\n<v>" |> Grid.new_from_string()

  def cross([hd]), do: hd

  def cross([hd | tail]) do
    for a <- hd, b <- Enum.map(cross(tail), fn b -> [a] ++ [b] end) do
      List.flatten(b)
    end
  end
end
