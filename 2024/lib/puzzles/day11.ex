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
    Enum.reduce(stones, Map.new(), fn
      {0, count}, acc ->
        update(acc, 1, count)

      {stone, count}, acc ->
        case Integer.digits(stone) do
          even when rem(length(even), 2) == 0 ->
            {l, r} = Enum.split(even, div(length(even), 2))
            acc |> update(Integer.undigits(l), count) |> update(Integer.undigits(r), count)

          _ ->
            update(acc, stone * 2024, count)
        end
    end)
    |> blink(step - 1)
  end

  defp update(map, key, value), do: Map.update(map, key, value, &(&1 + value))
end
