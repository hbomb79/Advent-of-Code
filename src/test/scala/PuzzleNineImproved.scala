// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
// import puzzles.PuzzleNine
import puzzles.PuzzleNineImproved.calcFollowMovement
import puzzles.PuzzleNineImproved.Point
class PuzzleNineImproved extends munit.FunSuite {
  test(
    "Tail two steps away from the head on the X axis will move to become adjacent"
  ) {
    val head = Point(3, 1)
    val tail = Point(1, 1)
    val out = calcFollowMovement(tail, head)(tail)
    assertEquals(out.x, 2)
    assertEquals(out.y, 1)
  }

  test(
    "Tail two steps away from the head on the Y axis will move to become adjacent"
  ) {
    val head = Point(1, 3)
    val tail = Point(1, 1)
    val out = calcFollowMovement(tail, head)(tail)
    assertEquals(out.x, 1)
    assertEquals(out.y, 2)
  }

  test(
    "Tail two steps away from the head diagonally (vertically) moves to become adjacent"
  ) {
    /*
     * _ _ _ _
     * _ _ H _
     * _ _ _ _
     * _ T _ _
     *
     * T should move to cell
     * underneath H
     */
    val head = Point(2, 1)
    val tail = Point(1, 3)
    val out = calcFollowMovement(tail, head)(tail)
    assertEquals(out.x, 2)
    assertEquals(out.y, 2)
  }

  test(
    "Tail two steps away from the head diagonally (horizontally) moves to become adjacent"
  ) {
    /*
     * _ _ _ _ _
     * _ _ _ _ _
     * _ _ _ H _
     * _ T _ _ _
     *
     * T should move to cell
     * to the left of H
     */
    val head = Point(3, 2)
    val tail = Point(1, 3)
    val out = calcFollowMovement(tail, head)(tail)
    assertEquals(out.x, 2)
    assertEquals(out.y, 2)
  }
}
