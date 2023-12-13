package puzzles
import main.Puzzle
import scala.annotation.tailrec

object PuzzleTwelve extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val map = HeightMap(fileLines.mkString.toCharArray(), fileLines(0).length)
    val currentPosition = map.content.indexWhere(_ == 'S')
    val targetPosition = map.content.indexWhere(_ == 'E')
    val outNode = findShortestPath(
      map,
      targetPosition,
      Seq(Node(currentPosition, 0)),
      Set.empty
    )

    println(outNode.map(_.steps))
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val map = HeightMap(fileLines.mkString.toCharArray(), fileLines(0).length)
    val startingPositions =
      map.content.zipWithIndex.filter((a, b) => a == 'S' || a == 'a').map(_._2)
    val targetPosition = map.content.indexWhere(_ == 'E')
    val outNodes =
      startingPositions
        .map(startPosition =>
          findShortestPath(
            map,
            targetPosition,
            Seq(Node(startPosition, 0)),
            Set.empty
          )
        )
        .flatten
        .map(_.steps)
        .min

    println(outNodes)
  }

  @tailrec
  def findShortestPath(
      map: HeightMap,
      targetPosition: Int,
      queue: Seq[Node],
      visited: Set[Int]
  ): Option[Node] = {
    queue match {
      case Nil => None
      case node :: _ if node.index == targetPosition =>
        Some(node)
      case node :: rest =>
        val currentIndex = node.index
        val possibleMoves = map
          .getMovesExcluding(currentIndex, visited)
          .filterNot(i => map.getAt(i) - map.getAt(currentIndex) > 1)
          .map(move => Node(move, node.steps + 1))

        findShortestPath(
          map,
          targetPosition,
          rest ++ possibleMoves,
          visited ++ possibleMoves.map(_.index)
        )
    }
  }

  case class Node(index: Int, steps: Int)
  case class HeightMap(content: Seq[Char], width: Int) {
    def getAt(index: Int): Char =
      content(index) match {
        case 'S' => 'a'
        case 'E' => 'z'
        case x   => x
      }

    def getMoves(index: Int): Seq[Int] = {
      val above = if (index <= width) None else Some(index - width)
      val below =
        if (index >= content.length - width) None else Some(index + width)
      val left = if (index % width == 0) None else Some(index - 1)
      val right = if (index % width == width - 1) None else Some(index + 1)

      Seq(
        above,
        left,
        below,
        right
      ).flatten
    }

    def getMovesExcluding(index: Int, excluding: Set[Int]): Seq[Int] =
      getMoves(index).filterNot(excluding.contains)
  }
}
