defmodule GridTest do
  use ExUnit.Case

  test "Grid.new" do
    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 0}, {:y, 1}, {:data, "1"}],
        [{:x, 1}, {:y, 1}, {:data, "2"}],
        [{:x, 2}, {:y, 1}, {:data, "3"}]
      ])

    assert map_size(grid[:data]) == 6
    assert grid[:width] == 3
    assert grid[:height] == 2

    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 1}, {:data, "1"}],
        [{:x, 6}, {:y, 2}, {:data, "2"}],
        [{:x, 7}, {:y, 3}, {:data, "3"}]
      ])

    assert map_size(grid[:data]) == 6
    assert grid[:width] == 8
    assert grid[:height] == 4
  end

  test "Grid.at_coord!" do
    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 1}, {:data, "1"}],
        [{:x, 6}, {:y, 2}, {:data, "2"}],
        [{:x, 7}, {:y, 3}, {:data, "3"}]
      ])

    assert map_size(grid[:data]) == 6
    assert grid[:width] == 8
    assert grid[:height] == 4

    assert Grid.at_coord!(grid, {2, 0}) == "C"
  end

  test "Grid.shift" do
    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 1}, {:data, "1"}],
        [{:x, 6}, {:y, 2}, {:data, "2"}],
        [{:x, 7}, {:y, 3}, {:data, "3"}]
      ])

    assert map_size(grid[:data]) == 6
    assert grid[:width] == 8
    assert grid[:height] == 4

    # Happy cases

    assert Grid.shift(grid, :up, {5, 1}) == {:ok, {5, 0}}
    assert Grid.shift!(grid, :up, {5, 1}) == {5, 0}

    assert Grid.shift(grid, :left, {5, 1}) == {:ok, {4, 1}}
    assert Grid.shift!(grid, :left, {5, 1}) == {4, 1}

    assert Grid.shift(grid, :right, {5, 1}) == {:ok, {6, 1}}
    assert Grid.shift!(grid, :right, {5, 1}) == {6, 1}

    assert Grid.shift(grid, :down, {5, 1}) == {:ok, {5, 2}}
    assert Grid.shift!(grid, :down, {5, 1}) == {5, 2}

    # Unhappy cases
    assert Grid.shift(grid, :up, {5, 0}) == {:error, :out_of_bounds}
    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.shift!(grid, :up, {5, 0}) end

    assert Grid.shift(grid, :left, {0, 3}) == {:error, :out_of_bounds}
    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.shift!(grid, :left, {0, 3}) end

    assert Grid.shift(grid, :right, {7, 3}) == {:error, :out_of_bounds}
    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.shift!(grid, :right, {7, 3}) end

    assert Grid.shift(grid, :down, {7, 3}) == {:error, :out_of_bounds}
    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.shift!(grid, :down, {7, 3}) end
  end

  test "Grid.find_next" do
    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 5}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 10}, {:data, "1"}],
        [{:x, 6}, {:y, 11}, {:data, "2"}],
        [{:x, 7}, {:y, 12}, {:data, "3"}]
      ])

    assert map_size(grid[:data]) == 6
    assert grid[:width] == 8
    assert grid[:height] == 13

    assert Grid.find_next!(grid, :down, {5, 0}) == {{5, 10}, "1"}
    assert Grid.find_next!(grid, :up, {5, 10}) == {{5, 0}, "C"}
    assert Grid.find_next!(grid, :left, {5, 0}) == {{1, 0}, "B"}
    assert Grid.find_next!(grid, :right, {0, 0}) == {{1, 0}, "B"}

    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.find_next!(grid, :right, {5, 0}) end
  end
end
