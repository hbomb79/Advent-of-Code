// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import puzzles.PuzzleNine._
class MySuite extends munit.FunSuite {

  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("Point two steps away on X axis moves closer") {
    val head = Point(3, 1)
    val tail = Point(1, 1)
    val out = tail.moveTowards(head)
    assertEquals(out.x, 2)
    assertEquals(out.y, 1)
  }

  test("Point two steps away on Y axis moves closer") {
    val head = Point(1, 3)
    val tail = Point(1, 1)
    val out = tail.moveTowards(head)
    assertEquals(out.x, 1)
    assertEquals(out.y, 2)
  }

  test("Point two steps away diagonally (vert) moves closer") {
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
    val out = tail.moveTowards(head)
    assertEquals(out.x, 2)
    assertEquals(out.y, 2)
  }

  test("Point two steps away diagonally (horiz) moves closer") {
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
    val out = tail.moveTowards(head)
    assertEquals(out.x, 2)
    assertEquals(out.y, 2)
  }
}
