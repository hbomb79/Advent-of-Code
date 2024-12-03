defmodule Mix.Tasks.Solve do
  use Mix.Task

  @shortdoc "Puzzle runner"
  def run(args) do
    {opts, _} =
      OptionParser.parse!(args,
        strict: [day: :string, part: :integer, test: :boolean],
        aliases: [t: :test, p: :part, d: :day]
      )

    day = Keyword.fetch!(opts, :day)
    mod = Module.concat([Puzzles, "Day#{day}"])

    input =
      File.read!(
        cond do
          opts[:test] -> "lib/puzzles/input/day#{day}-example.txt"
          true -> "lib/puzzles/input/day#{day}.txt"
        end
      )

    if Keyword.get(opts, :part, 1) == 1 do
      mod.part1(input) |> IO.inspect(label: "Part 1 Results")
    end

    if Keyword.get(opts, :part, 2) == 2 do
      mod.part2(input) |> IO.inspect(label: "Part 2 Results")
    end
  end
end
