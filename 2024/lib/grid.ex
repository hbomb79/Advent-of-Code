defmodule Grid do
  @type coordinate() :: {integer(), integer()}

  defstruct(data: [], width: 0, height: 0)
  @type t :: %__MODULE__{data: %{coordinate() => any()}, width: integer(), height: integer()}

  @spec new([[x: integer(), y: integer(), data: any()]]) :: t()
  def new(cells) do
    {data, dims} =
      List.foldl(cells, {%{}, [{:x1, 0}, {:x2, 0}, {:y1, 0}, {:y2, 0}]}, fn cell, {data, dims} ->
        {
          Map.put(data, {cell[:x], cell[:y]}, cell[:data]),
          [
            {:x1, min(dims[:x1], cell[:x])},
            {:x2, max(dims[:x2], cell[:x])},
            {:y1, min(dims[:y1], cell[:y])},
            {:y2, max(dims[:y2], cell[:y])}
          ]
        }
      end)

    %__MODULE__{data: data, width: 1 + dims[:x2] - dims[:x1], height: 1 + dims[:y2] - dims[:y1]}
  end

  @spec at_coord!(Grid.t(), coordinate()) :: any()
  def at_coord!(grid, {x, y}) do
    Map.fetch!(grid[:data], {x, y})
  end

  @spec shift(Grid.t(), :down | :left | :right | :up, coordinate()) ::
          {:error, :out_of_bounds} | {:ok, coordinate()}
  def shift(_, :left, {x, y}) do
    if x - 1 < 0 do
      {:error, :out_of_bounds}
    else
      {:ok, {x - 1, y}}
    end
  end

  def shift(grid, :right, {x, y}) do
    if x + 1 >= grid[:width] do
      {:error, :out_of_bounds}
    else
      {:ok, {x + 1, y}}
    end
  end

  def shift(_, :up, {x, y}) do
    if y - 1 < 0 do
      {:error, :out_of_bounds}
    else
      {:ok, {x, y - 1}}
    end
  end

  def shift(grid, :down, {x, y}) do
    if y + 1 >= grid[:height] do
      {:error, :out_of_bounds}
    else
      {:ok, {x, y + 1}}
    end
  end

  @spec shift!(Grid.t(), :down | :left | :right | :up, coordinate()) :: coordinate()
  def shift!(grid, dir, coordinate) do
    case shift(grid, dir, coordinate) do
      {:ok, val} -> val
      {:error, err} -> raise Atom.to_string(err)
    end
  end

  @spec find_next!(Grid.t(), :down | :left | :right | :up, coordinate()) :: {coordinate(), any()}
  def find_next!(grid, dir, coordinate) do
    p = shift!(grid, dir, coordinate)

    case Map.fetch(grid[:data], p) do
      {:ok, cell} -> {p, cell}
      _ -> find_next!(grid, dir, p)
    end
  end
end
