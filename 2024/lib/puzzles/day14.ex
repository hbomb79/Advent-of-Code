defmodule Puzzles.Day14 do
  def part1(input) do
    {robots, width, height} = parse(input)

    hw = div(width, 2)
    hh = div(height, 2)

    buckets =
      Enum.reduce(robots, [], fn robot, acc ->
        move(100, {width, height}, robot) |> bucket(acc, hw, hh)
      end)

    buckets[:tl] * buckets[:bl] * buckets[:tr] * buckets[:br]
  end

  def part2(input) do
    {robots, width, height} = parse(input)
    find_tree(1, robots, {width, height}, length(robots))
  end

  defp bucket({{x, y}, _vel}, buckets, w, h) do
    bucket =
      cond do
        x < w && y < h -> :tl
        x > w && y > h -> :br
        x < w && y > h -> :bl
        x > w && y < h -> :tr
        true -> :boundary
      end

    Keyword.update(buckets, bucket, 1, &(&1 + 1))
  end

  def find_tree(idx, robots, dims, num) do
    # Assume tree drawing requires all robots to be at unique positions
    robots = Enum.map(robots, fn robot -> move(1, dims, robot) end)

    cond do
      MapSet.new(robots, &elem(&1, 0)) |> MapSet.size() == num -> idx
      true -> find_tree(idx + 1, robots, dims, num)
    end
  end

  defp move(steps, {width, height}, {{px, py}, {vx, vy} = vel}) do
    px = rem(px + vx * steps, width + 1)
    py = rem(py + vy * steps, height + 1)

    px = if px < 0, do: width + 1 + px, else: px
    py = if py < 0, do: height + 1 + py, else: py

    {{px, py}, vel}
  end

  defp parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.reduce({[], 0, 0}, fn str, {acc, maxx, maxy} ->
      [px, py, vx, vy] =
        Regex.scan(~r/\-?\d+/, str) |> List.flatten() |> Enum.map(&String.to_integer/1)

      {[{{px, py}, {vx, vy}} | acc], max(maxx, px), max(maxy, py)}
    end)
  end
end
