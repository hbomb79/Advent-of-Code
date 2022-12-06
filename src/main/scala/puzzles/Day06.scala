package puzzles
import main.Puzzle
import scala.annotation.tailrec

object PuzzleSix extends Puzzle {
  override def partOne(lines: Seq[String]): Unit = println(
    findMarkerPosition(lines.head, 4)
  )

  override def partTwo(lines: Seq[String]): Unit = println(
    findMarkerPosition(lines.head, 14)
  )

  private def findMarkerPosition(stream: String, size: Int): Option[Int] = {
    (0 to stream.length)
      .find(pos => stream.slice(pos, pos + size).distinct.length == size)
      .map(_ + size)
  }
}
