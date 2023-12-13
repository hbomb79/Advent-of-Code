package puzzles

import main.Puzzle

object PuzzleTwenty extends Puzzle {
  val DecryptionKey = 811589153
  override def partOne(fileLines: Seq[String]): Unit = {
    val lines = fileLines.map(_.toLong).toList
    println(findGroove(mixContents(lines, 1)))
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val lines = fileLines.map(_.toLong).map(_ * DecryptionKey).toList
    println(findGroove(mixContents(lines, 10)))
  }

  private def findGroove(list: List[Long]): Long = {
    val base = list.indexOf(0)
    List(1000, 2000, 3000)
      .map(_ + base)
      .map(_ % list.size)
      .map(list.apply)
      .sum
  }

  private def mixContents(input: List[Long], n: Int): List[Long] = {
    val list = input.zipWithIndex
    (0 until n)
      .foldLeft(list) { case (a, _) =>
        list.indices.foldLeft(a) { case (acc, listIndex) =>
          val shift = input(listIndex)
          val accIdx = acc.indexWhere { case (_, i) => i == listIndex }

          val (beforeItem, afterItem) = acc.splitAt(accIdx)
          val listRemoved = beforeItem ++ afterItem.drop(1)

          val dest = ((accIdx + shift) % (acc.length - 1)) match {
            case d if d <= 0 => (acc.length - 1 + d).toInt
            case d           => d.toInt
          }

          val (beforeDest, afterDest) = listRemoved.splitAt(dest)
          (beforeDest :+ (shift, listIndex)) ++ afterDest
        }
      }
      .map(_._1)
  }
}
