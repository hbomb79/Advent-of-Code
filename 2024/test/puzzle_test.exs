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

  test "D10" do
    assert Runner.run("10", 1) == 667
    assert Runner.run("10", 2) == 1344
  end

  test "D11" do
    assert Runner.run("11", 1) == 229_043
    assert Runner.run("11", 2) == 272_673_043_446_478
  end

  test "D12" do
    assert Runner.run("12", 1) == 1_457_298
    assert Runner.run("12", 2) == 921_636
  end

  test "D13" do
    assert Runner.run("13", 1) == 29023.0
    assert Runner.run("13", 2) == 96_787_395_375_634.0
  end

  test "D14" do
    assert Runner.run("14", 1) == 228_457_125
    assert Runner.run("14", 2) == 6493
  end

  test "D15" do
    assert Runner.run("15", 1) == 1_538_871
    assert Runner.run("15", 2) == 1_543_338
  end

  test "D16" do
    assert Runner.run("16", 1) == 106_512
    assert Runner.run("16", 2) == 563
  end

  test "D17" do
    assert Runner.run("17", 1) == "1,4,6,1,6,4,3,0,3"
    assert Runner.run("17", 2) == 265_061_364_597_659
  end

  test "D18" do
    assert Runner.run("18", 1) == 278
    assert Runner.run("18", 2) == {43, 12}
  end

  test "D19" do
    assert Runner.run("19", 1) == 238
    assert Runner.run("19", 2) == 635_018_909_726_691
  end

  test "D20" do
    assert Runner.run("20", 1) == 1459
    assert Runner.run("20", 2) == 1_016_066
  end

  test "D21" do
    assert Runner.run("21", 1) == 156_714
    assert Runner.run("21", 2) == 191_139_369_248_202
  end

  test "D22" do
    assert Runner.run("22", 1) == 20_071_921_341
    assert Runner.run("22", 2) == 2242
  end

  test "D23" do
    assert Runner.run("23", 1) == 1046
    assert Runner.run("23", 2) == "de,id,ke,ls,po,sn,tf,tl,tm,uj,un,xw,yz"
  end
end
