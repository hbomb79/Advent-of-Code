package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleEighteen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val cubes = fileLines.collect { case s"$x,$y,$z" =>
      Point(x.toInt, y.toInt, z.toInt)
    }

    val blockedFaces = for {
      p <- cubes
      q <- cubes
      if p != q && p.isTouching(q)
    } yield 1

    val allFaces = cubes.size * 6
    println(allFaces - blockedFaces.sum)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val cubes = fileLines.collect { case s"$x,$y,$z" =>
      Point(x.toInt, y.toInt, z.toInt)
    }

    val xRange = (cubes.map(_.x).min) - 1 to (cubes.map(_.x).max + 1)
    val yRange = (cubes.map(_.y).min) - 1 to (cubes.map(_.y).max + 1)
    val zRange = (cubes.map(_.z).min) - 1 to (cubes.map(_.z).max + 1)
    val start = Point(xRange.min, yRange.min, zRange.min)
    val ans = search(
      List(start),
      Set.empty,
      0,
      cubes.toSet,
      (point: Point) =>
        xRange.contains(point.x) &&
          yRange.contains(point.y) &&
          zRange.contains(point.z)
    )
    println(ans)
  }

  @tailrec
  private def search(
      queue: List[Point],
      visited: Set[Point],
      exposedSides: Int,
      cubes: Set[Point],
      inBounds: Point => Boolean
  ): Int = queue match {
    case Nil => exposedSides
    case current :: next =>
      val (blocked, air) = current.edges
        .filter(inBounds)
        .filterNot(visited)
        .filterNot(next.contains)
        .partition(cubes.contains)

      search(
        next ++ air,
        visited + current,
        exposedSides + blocked.size,
        cubes,
        inBounds
      )

  }

  case class Point(x: Int, y: Int, z: Int) {
    def isTouching(other: Point): Boolean = {
      val dX = Math.abs(x - other.x)
      val dY = Math.abs(y - other.y)
      val dZ = Math.abs(z - other.z)

      List(dX, dY, dZ).sorted == List(0, 0, 1)
    }

    def edges: Seq[Point] = Seq(
      copy(x = x + 1),
      copy(y = y + 1),
      copy(z = z + 1),
      copy(x = x - 1),
      copy(y = y - 1),
      copy(z = z - 1)
    )
  }
}
