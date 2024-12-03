defmodule Mix.Tasks.Create do
  use Mix.Task

  @shortdoc "Puzzle boilerplate generator"
  @requirements ["app.start"]
  def run([day]) do
    token = File.read!("token.txt") |> String.trim_trailing()
    dayNum = String.to_integer(day)

    puzzleInput =
      Req.get!("https://adventofcode.com/2024/day/#{dayNum}/input",
        headers: %{Cookie: "session=#{token}"}
      ).body

    # Create puzzle input files
    Mix.Generator.create_file("lib/puzzles/input/day#{day}.txt", puzzleInput)
    Mix.Generator.create_file("lib/puzzles/input/day#{day}-example.txt", "fillmeupdaddy")

    # Create boilerplate module file
    puzzleBoilerplate = """
    defmodule Puzzles.Day#{day} do
      def part1(_input) do
      end

      def part2(_input) do
      end
    end
    """

    Mix.Generator.create_file("lib/puzzles/day#{day}.ex", puzzleBoilerplate)
  end
end
