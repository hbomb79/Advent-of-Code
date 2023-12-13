import puzzles.PuzzleTwentyTwo.{Map, Point, Direction, Row, Turn, Move}
class PuzzleTwentyTwo extends munit.FunSuite {
  def standardMap(
      start: Point = Point(6, 1),
      direction: Direction = Direction.E
  ) = Map(
    start,
    direction,
    /* Map
     * S = Starting point
     *
     * S . # .
     * . # # .
     * . . . .
     * . # . .
     */
    Seq(
      Row(5, Seq(true, true, false, true)),
      Row(5, Seq(true, false, false, true)),
      Row(5, Seq(true, true, true, true)),
      Row(5, Seq(true, false, true, true))
    )
  )

  test("Easterly wrapping works when unblocked") {
    val point = standardMap(Point(6, 3)).executeInstruction(Move(5)).position
    assertEquals(point.x, 7)
    assertEquals(point.y, 3)
  }
  test("Easterly wrapping works when blocked") {}

  test("Southerly wrapping works when unblocked") {}
  test("Southerly wrapping works when blocked") {}

  test("Northerly wrapping works when unblocked") {}
  test("Northerly wrapping works when blocked") {}

  test("Westerly wrapping works when unblocked") {}
  test("Westerly wrapping works when blocked") {}
}
