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

  test "D4" do
    assert Runner.run("04", 1) == 2434
    assert Runner.run("04", 2) == 1835
  end

  test "D5" do
    assert Runner.run("05", 1) == 5713
    assert Runner.run("05", 2) == 5180
  end

  test "D6" do
    assert Runner.run("06", 1) == 4789
    assert Runner.run("06", 2) == 1304
  end

  test "D7" do
    assert Runner.run("07", 1) == 663_613_490_587
    assert Runner.run("07", 2) == 110_365_987_435_001
  end

  test "D8" do
    assert Runner.run("08", 1) == 327
    assert Runner.run("08", 2) == 1233
  end

  test "D9" do
    assert Runner.run("09", 1) == 6_607_511_583_593
    assert Runner.run("09", 2) == 6_636_608_781_232
  end
end
