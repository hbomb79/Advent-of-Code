defmodule Aoc2024 do
  @moduledoc """
  Documentation for `Aoc2024`.
  """

  def run_puzzle(args, puzzleMap) do
    {opts, _} =
      OptionParser.parse!(args,
        strict: [
          day: :string,
          part: :integer,
          test: :boolean
        ],
        aliases: [t: :test, p: :part, d: :day]
      )

    day = "day" <> opts[:day]
    mod = Map.fetch!(puzzleMap, day)

    input =
      cond do
        opts[:test] -> File.read!("lib/puzzles/input/" <> day <> "-example.txt")
        true -> File.read!("lib/puzzles/input/" <> day <> ".txt")
      end

    if opts[:part] == nil || opts[:part] == 1 do
      mod.part1(input) |> IO.inspect(label: "Part 1 Results")
    end

    if opts[:part] == nil || opts[:part] == 2 do
      mod.part2(input) |> IO.inspect(label: "Part 2 Results")
    end
  end
end
