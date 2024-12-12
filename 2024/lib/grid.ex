defmodule Grid do
  @type coordinate() :: {integer(), integer()}

  defstruct(data: [], width: 0, height: 0)
  @type t() :: %__MODULE__{data: %{coordinate() => any()}, width: integer(), height: integer()}

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

  @spec new_from_lines([String.t()]) :: t()
  def new_from_lines(lines) do
    Enum.with_index(lines)
    |> List.foldr([], fn {line, y}, acc ->
      String.graphemes(line)
      |> Enum.with_index()
      |> List.foldr(acc, fn {data, x}, acc -> [[{:x, x}, {:y, y}, {:data, data}] | acc] end)
    end)
    |> new
  end

  @spec new_from_string(String.t()) :: t()
  def new_from_string(string) do
    Input.parse_lines(string) |> new_from_lines()
  end

  def map(grid, fun) do
    new_data = Map.new(grid.data, fun)
    struct!(grid, data: new_data)
  end

  @spec point(t(), coordinate()) :: :error | {:ok, any()}
  def point(grid, {x, y}) do
    Map.fetch(grid.data, {x, y})
  end

  @spec point!(t(), coordinate()) :: any()
  def point!(grid, {x, y}) do
    Map.fetch!(grid.data, {x, y})
  end

  @spec find_value!(t(), any()) :: {coordinate(), any}
  def find_value!(grid, data) do
    case find_value(grid, data) do
      {:error, err} -> raise Atom.to_string(err)
      {:ok, d} -> d
    end
  end

  @spec find_value(t(), any()) :: {:error, :not_found} | {:ok, {coordinate(), any}}
  def find_value(grid, data) do
    case Enum.find(grid.data, fn {_, d} -> d == data end) do
      nil -> {:error, :not_found}
      d -> {:ok, d}
    end
  end

  @doc """
  Returns all neighbouring points from the point provided. Any directions that
  are out-of-bounds will be omitted. Neighbours are only returned for adjacent cells
  of the grid (i.e. +/- 1)
  """
  @spec neighbours(t(), coordinate()) :: [coordinate()]
  def neighbours(grid, point) do
    [
      shift(grid, :up, point),
      shift(grid, :down, point),
      shift(grid, :left, point),
      shift(grid, :right, point)
    ]
    |> List.foldl([], fn
      {:error, _}, acc -> acc
      {:ok, point}, acc -> [point | acc]
    end)
  end

  @doc """
  Given a point in the provided grid, this function travels outwardly in the direction
  provided for the number of points specified by dist. The points will be returned in same order
  they are encountered.

  Any points encountered which fall out of range of the grid will be discarded, but no error will be
  raised.

  The origin point WILL be included in the return value
  """
  @spec cast_ray(
          t(),
          coordinate(),
          :down | :down_left | :down_right | :left | :right | :up | :up_left | :up_right,
          integer()
        ) :: [coordinate()]
  def cast_ray(grid, {x, y}, dir, dist) do
    {dx, dy} = dir_to_delta(dir)

    List.foldl(Enum.to_list(0..dist), [], fn mul, acc ->
      point = {x + dx * mul, y + dy * mul}
      if is_oob(grid, point), do: acc, else: [point | acc]
    end)
    |> Enum.reverse()
  end

  @spec shortest_path(
          t(),
          coordinate(),
          coordinate(),
          AStar.cost_fn(),
          AStar.cost_fn()
        ) :: list()
  @spec shortest_path(
          t(),
          coordinate(),
          coordinate(),
          AStar.cost_fn(),
          AStar.cost_fn(),
          (coordinate() -> boolean())
        ) :: list()
  @doc """
  Given a grid and a starting point, this function will find the shortest path from the
  starting point to a cell containing the target DATA. The path is calculated using A*.

  The functions to calculate the h and g costs of a given node are the callers responsibility, as they
  depend on the context of the data stored in the grid. These functions will be provided with the
  coordinates of the vertex under test; the data can be obtained using [Grid.point/2]

  A visited set is maintained to ensure no cell is visited more than once. Additionally,
  an optional neighbour_predicate can be provided to exclude certain neighbouring cells.

  Once a path is found, {:ok, path} will be returned, where path is a list of coordinates.

  If a path could not be found, {:error} will be returned instead,
  """
  def shortest_path(grid, current, target, h, g, neighbour_predicate \\ fn _ -> true end) do
    ns_fn = &(neighbours(grid, &1) |> Enum.filter(fn n -> neighbour_predicate.(n) end))
    goal_fn = &(point!(grid, &1) == target)

    AStar.shortest_path({ns_fn, g, h}, current, goal_fn)
  end

  @spec filter_data(t(), [any()]) :: t()
  def filter_data(grid, allowed_data),
    do: filter_fn(grid, fn {_, data} -> data in allowed_data end)

  @spec filter_fn(any(), (any() -> boolean())) :: t()
  def filter_fn(grid, predicate) do
    Enum.filter(grid.data, predicate)
    |> Enum.map(fn {{x, y}, data} -> [{:x, x}, {:y, y}, {:data, data}] end)
    |> new
  end

  @spec shift(
          t(),
          :down | :down_left | :down_right | :left | :right | :up | :up_left | :up_right,
          coordinate()
        ) :: {:error, :out_of_bounds} | {:ok, {number(), number()}}
  def shift(grid, dir, {x, y}) do
    {dx, dy} = dir_to_delta(dir)
    point = {x + dx, y + dy}
    if is_oob(grid, point), do: {:error, :out_of_bounds}, else: {:ok, point}
  end

  @spec shift!(t(), :down | :left | :right | :up, coordinate()) :: coordinate()
  def shift!(grid, dir, coordinate) do
    case shift(grid, dir, coordinate) do
      {:ok, val} -> val
      {:error, err} -> raise Atom.to_string(err)
    end
  end

  @spec find_next!(t(), :down | :left | :right | :up, coordinate()) :: {coordinate(), any()}
  def find_next!(grid, dir, coordinate) do
    p = shift!(grid, dir, coordinate)

    case Map.fetch(grid.data, p) do
      {:ok, cell} -> {p, cell}
      _ -> find_next!(grid, dir, p)
    end
  end

  def rotate_dir(dir, :clockwise) do
    case dir do
      :up -> :right
      :down -> :left
      :left -> :up
      :right -> :down
    end
  end

  def rotate_dir(dir, :anticlockwise) do
    case dir do
      :up -> :left
      :down -> :right
      :left -> :down
      :right -> :up
    end
  end

  def dir_to_delta(dir) do
    case dir do
      :up -> {0, -1}
      :down -> {0, 1}
      :left -> {-1, 0}
      :right -> {1, 0}
      :up_left -> {-1, -1}
      :up_right -> {1, -1}
      :down_left -> {-1, 1}
      :down_right -> {1, 1}
    end
  end

  def dir_opposite(dir) do
    case dir do
      :up -> :down
      :down -> :up
      :left -> :right
      :right -> :left
      :up_left -> :down_right
      :up_right -> :down_left
      :down_left -> :up_right
      :down_right -> :up_left
    end
  end

  def delta_to_dir(dx, dy) do
    case {dx, dy} do
      {0, -1} -> :up
      {0, 1} -> :down
      {-1, 0} -> :left
      {1, 0} -> :right
      {-1, -1} -> :up_left
      {1, -1} -> :up_right
      {-1, 1} -> :down_left
      {1, 1} -> :down_right
    end
  end

  def is_oob(grid, {x, y}) do
    x < 0 || x >= grid.width || y < 0 || y >= grid.height
  end
end
