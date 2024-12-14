defmodule Puzzles.Day14 do
  def part1(input) do
    {robots, width, height} = parse(input)

    r =
      robots
      |> Enum.map(fn robot ->
        move(100, {width, height}, robot)
      end)
      |> Enum.map(fn {{px, py}, _vel} ->
        # Calculate which quandrant the robot is in
        w = div(width, 2)
        h = div(height, 2)

        cond do
          px < w && py < h -> :tl
          px > w && py > h -> :br
          px < w && py > h -> :bl
          px > w && py < h -> :tr
          true -> :boundary
        end
      end)

    Enum.count(r, &(&1 == :tl)) *
      Enum.count(r, &(&1 == :bl)) *
      Enum.count(r, &(&1 == :tr)) *
      Enum.count(r, &(&1 == :br))
  end

  def part2(_input) do
  end

  defp move(steps, {width, height}, {{px, py}, {vx, vy} = vel}) do
    vx = vx * steps
    vy = vy * steps

    px = rem(px + vx, width + 1)
    py = rem(py + vy, height + 1)

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
