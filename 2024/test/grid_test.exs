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

    assert map_size(grid.data) == 6
    assert grid.width == 3
    assert grid.height == 2

    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 1}, {:data, "1"}],
        [{:x, 6}, {:y, 2}, {:data, "2"}],
        [{:x, 7}, {:y, 3}, {:data, "3"}]
      ])

    assert map_size(grid.data) == 6
    assert grid.width == 8
    assert grid.height == 4
  end

  test "Grid.new_from_string" do
    grid = Grid.new_from_string("ABCD\n1234\nXOXO\n")
    assert grid.width == 4
    assert grid.height == 3

    assert Grid.new_from_lines(["ABCD", "1234", "XOXO"]) == grid

    # top left
    assert Grid.point!(grid, {0, 0}) == "A"
    # mid
    assert Grid.point!(grid, {3, 1}) == "4"
    # bot right
    assert Grid.point!(grid, {3, 2}) == "O"
  end

  test "Grid.point!" do
    grid =
      Grid.new([
        [{:x, 0}, {:y, 0}, {:data, "A"}],
        [{:x, 1}, {:y, 0}, {:data, "B"}],
        [{:x, 2}, {:y, 0}, {:data, "C"}],
        [{:x, 5}, {:y, 1}, {:data, "1"}],
        [{:x, 6}, {:y, 2}, {:data, "2"}],
        [{:x, 7}, {:y, 3}, {:data, "3"}]
      ])

    assert map_size(grid.data) == 6
    assert grid.width == 8
    assert grid.height == 4

    assert Grid.point!(grid, {2, 0}) == "C"
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

    assert map_size(grid.data) == 6
    assert grid.width == 8
    assert grid.height == 4

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

    assert map_size(grid.data) == 6
    assert grid.width == 8
    assert grid.height == 13

    assert Grid.find_next!(grid, :down, {5, 0}) == {{5, 10}, "1"}
    assert Grid.find_next!(grid, :up, {5, 10}) == {{5, 0}, "C"}
    assert Grid.find_next!(grid, :left, {5, 0}) == {{1, 0}, "B"}
    assert Grid.find_next!(grid, :right, {0, 0}) == {{1, 0}, "B"}

    assert_raise RuntimeError, "out_of_bounds", fn -> Grid.find_next!(grid, :right, {5, 0}) end
  end

  test "Grid.neighbours" do
    grid = Grid.new_from_string("ABCD\n1234\nXOXO")
    assert Grid.neighbours(grid, {1, 1}) -- [{0, 1}, {1, 0}, {1, 2}, {2, 1}] == []
    assert Grid.neighbours(grid, {0, 0}) -- [{0, 1}, {1, 0}] == []
    assert Grid.neighbours(grid, {3, 2}) -- [{2, 2}, {3, 1}] == []
  end

  test "Grid.filter" do
    grid = Grid.new_from_string("ABCD\n1234\nXOXO")
    grid2 = Grid.filter_data(grid, ["A", "B", "C", "D", "X", "O"])

    assert grid != grid2
    assert grid2.width == 4
    assert grid2.height == 3
    assert Grid.point!(grid2, {1, 0}) == "B"
    assert Grid.point!(grid2, {1, 2}) == "O"
  end

  describe "Grid.shortest_path" do
    test "finds shortest path" do
      grid = Grid.new_from_string("ABCD\nEFGH\nIJKL\nMNOP")

      # Find 'J' point target (for use with h())
      {{tx, ty}, _} = Grid.find_value!(grid, "J")

      # Manhatten distance heuristic
      h = fn {x, y} -> abs(tx - x) + abs(ty - y) end

      # Constant cost for each node
      g = fn _ -> 1 end

      # Expected path: right -> down -> down
      assert Grid.shortest_path(grid, {0, 0}, "J", h, g) == [{1, 0}, {1, 1}, {1, 2}]
    end

    test "avoids high cost nodes" do
      grid = Grid.new_from_string("ABCD\nEFGH\nIJKL\nMNOP")

      # Find 'J' point target (for use with h())
      {{tx, ty}, _} = Grid.find_value!(grid, "J")

      # Manhatten distance heuristic
      h = fn {x, y} -> abs(tx - x) + abs(ty - y) end

      # Constant cost for each node, except for {1,1} which we'll increase to a high
      # value to discourage the algorithm from using it
      g = fn
        {1, 1} -> 10
        _ -> 1
      end

      # Expected path: down -> down -> right
      assert Grid.shortest_path(grid, {0, 0}, "J", h, g) == [{0, 1}, {0, 2}, {1, 2}]
    end
  end
end
