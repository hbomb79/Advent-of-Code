defmodule PuzzleTest do
  use ExUnit.Case

  test "D1" do
    assert Runner.run("01", 1) == 1_319_616
    assert Runner.run("01", 2) == 27_267_728
  end

  test "D2" do
    assert Runner.run("02", 1) == 442
    assert Runner.run("02", 2) == 493
  end

  test "D3" do
    assert Runner.run("03", 1) == 175_615_763
    assert Runner.run("03", 2) == 74_361_272
  end
end
