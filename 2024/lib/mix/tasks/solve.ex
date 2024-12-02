defmodule Mix.Tasks.Solve do
  use Mix.Task

  @shortdoc "Puzzle runner"
  def run(args) do
    Aoc2024.run_puzzle(
      args,
      Map.new([
        {"day01", Puzzles.Day01},
        {"day02", Puzzles.Day02}
      ])
    )
  end
end
