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

    if Keyword.get(opts, :part, 1) == 1 do
      Runner.run(day, 1, opts[:test]) |> IO.inspect(label: "Part 1 Results")
    end

    if Keyword.get(opts, :part, 2) == 2 do
      Runner.run(day, 2, opts[:test]) |> IO.inspect(label: "Part 2 Results")
    end
  end
end
