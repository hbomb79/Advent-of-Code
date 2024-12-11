defmodule Puzzles.Day11 do
  def part1(input), do: do_blinks(input, 25)
  def part2(input), do: do_blinks(input, 75)

  def do_blinks(input, steps) do
    String.split(input)
    |> Enum.map(&String.to_integer/1)
    |> Map.from_keys(1)
    |> blink(steps)
  end

  def blink(stones, 0), do: stones |> Map.values() |> Enum.sum()

  def blink(stones, step) do
    Enum.reduce(stones, Map.new(), fn {stone, count}, acc ->
      cond do
        stone == 0 ->
          map_add(acc, 1, count)

        rem(length(Integer.digits(stone)), 2) == 0 ->
          str = Integer.to_string(stone)
          {l, r} = String.split_at(str, div(String.length(str), 2))

          acc |> map_add(String.to_integer(l), count) |> map_add(String.to_integer(r), count)

        true ->
          map_add(acc, stone * 2024, count)
      end
    end)
    |> blink(step - 1)
  end

  defp map_add(map, key, value), do: Map.put(map, key, Map.get(map, key, 0) + value)
end
