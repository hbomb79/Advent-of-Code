package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleFourteen extends Puzzle {
    override def partOne(fileLines: Seq[String]): Unit = {
        //TODO
    }
    override def partTwo(fileLines: Seq[String]): Unit = {
        val l = parse(fileLines)
        println(l)

        println
        val n = runSim(Point(500, 0), l, Set.empty)
        println(n.size)
    }

    private def parse(lines: Seq[String]): Set[Point] =
        lines.foldLeft(Set.empty[Point]) { case (blockedPoints, line) =>
            blockedPoints ++ parseLine(extractPoints(line, Seq.empty), blockedPoints)
        }

    private def runSim(entryPoint: Point, wallPoints: Set[Point], sandPoints: Set[Point]): Set[Point] = {
        if(sandPoints.size % 100 == 0) {
            println(s"Simulating sand #${sandPoints.size}")
        }
        simulateSand(entryPoint, wallPoints, sandPoints) match {
            case Some(restPoint) if restPoint.x == entryPoint.x && restPoint.y == entryPoint.y =>
                sandPoints + restPoint
            case Some(restPoint) => runSim(entryPoint, wallPoints, sandPoints + restPoint)
            case None =>
                throw new IllegalStateException()
        }
    }

    @tailrec
    private def simulateSand(fallPoint: Point, blockedPoints: Set[Point], sandPoints: Set[Point]): Option[Point] = {
        // Test if fallPoint can move down, down left, or down right. If blocked by sand, or wall
        // will come to rest and return true. Otherwise, move the point and recurse
        val lowestY = blockedPoints.map(_.y).max + 2
        val down = fallPoint.copy(y = fallPoint.y + 1)
        val attemptMoves = Seq(down, down.copy(x = down.x-1), down.copy(x = down.x+1)).filter(_.y < lowestY)
        attemptMoves.find(a => !sandPoints.contains(a) && !blockedPoints.contains(a)) match {
            case None => 
                // No valid moves, all are blocked. Comes to a rest
                Some(fallPoint)
            case Some(Point(_, y)) if y > lowestY =>
                // Falling out of world 
                None
            case Some(point) =>
                // Move down
                simulateSand(point, blockedPoints, sandPoints)
        }
    }

    sealed case class Point(x: Int, y: Int)
    private def extractPoints(line: String, points: Seq[Point]): Seq[Point] = {
        val l = line.trim
        l match {
            case s if s.isEmpty => points
            case s if s.startsWith("->") =>
                // Start of arrow, consume
                extractPoints(s.drop(2), points)
            case _ =>
                // Should be a number pair, extract
                val first = l.takeWhile(_.isDigit)
                val second = l.drop(first.length + 1).takeWhile(_.isDigit)
                val rest = l.drop(first.length + second.length + 2)
                extractPoints(rest, points :+ Point(first.toInt,second.toInt))
        }
    }

    private def parseLine(points: Seq[Point], blocked: Set[Point]): Set[Point] = 
        points.sliding(2).foldLeft(Set.empty[Point]) {
            case (acc, Seq(_)) =>
                // Last element with no connecting point.
                acc
            case (acc, Seq(first, second)) =>
                if(first.x == second.x) {
                    // Moving along the Y axis (vertical line)
                    val ps = Seq(first.y, second.y)
                    val min = ps.min
                    val max = ps.max

                    val n = (min to max).map{ y => 
                        Point(first.x, y)
                    }

                    acc ++ n
                } else {
                    // Moving along the X axis (horizontal line)
                    val ps = Seq(first.x, second.x)
                    val min = ps.min
                    val max = ps.max

                    val n = (min to max).map{ x => 
                        Point(x, first.y)
                    }

                    acc ++ n
                }

        }
}
