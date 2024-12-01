defmodule Mix.Tasks.Solve do
  use Mix.Task

  @shortdoc "Puzzle runner"
  def run(args) do
    Aoc2024.run_puzzle(
      args,
      Map.new([
        {"day01", Puzzles.Day01}
      ])
    )
  end
end
