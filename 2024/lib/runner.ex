defmodule Runner do
  @spec run(String.t(), 1 | 2, boolean()) :: any()
  @spec run(String.t(), 1 | 2) :: any()
  def run(day, part, use_example \\ false) do
    mod = Module.concat([Puzzles, "Day#{day}"])

    input =
      File.read!(
        cond do
          use_example -> "lib/puzzles/input/day#{day}-example.txt"
          true -> "lib/puzzles/input/day#{day}.txt"
        end
      )

    case part do
      1 -> mod.part1(input)
      2 -> mod.part2(input)
    end
  end
end
