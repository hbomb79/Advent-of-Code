package puzzles

import main.Puzzle
import scala.util.matching.Regex
import scala.compiletime.ops.boolean

object PuzzleThirteen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    println(compare(parse(fileLines)).sum)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val lines = parse(fileLines).flatMap(a => Seq(a._1, a._2))
    val dividers = Seq(
      ContentList(Seq(ContentList(Seq(2)))),
      ContentList(Seq(ContentList(Seq(6))))
    )

    val sorted = sort(lines ++ dividers)
    val dividerIndex = dividers
      .map(d => sorted.indexOf(d) + 1)
      .reduce(_ * _)
    println(dividerIndex)
  }

  private def sort(content: Seq[Content]): Seq[Content] =
    content.sortWith((a, b) =>
      compareLists(Seq(a), Seq(b)) match {
        case Result.Ordered   => true
        case Result.Unordered => false
        case Result.Unknown =>
          throw new IllegalStateException(
            "Comparison of top-level list was unconclusive"
          )
          false
      }
    )

  private def compare(pairs: Seq[ListPairs]): Seq[Int] = {
    pairs.zipWithIndex.collect {
      case ((ContentList(l), ContentList(r)), index) =>
        compareLists(l, r) match {
          case Result.Ordered   => Some(index + 1)
          case Result.Unordered => None
          case Result.Unknown =>
            throw new IllegalStateException(
              "Comparison of top-level list was unconclusive"
            )
            None
        }
    }.flatten
  }

  private def compareLists(
      left: Seq[Content],
      right: Seq[Content]
  ): Result =
    (left, right) match {
      case (Nil, Nil) => Result.Unknown
      case (Nil, _)   => Result.Ordered
      case (_, Nil)   => Result.Unordered
      case (lH :: lRest, rH :: rRest) =>
        (lH, rH) match {
          case (ContentList(l), ContentList(r)) =>
            // Both items are lists... Compare each value
            compareLists(l, r) match {
              case Result.Unknown =>
                compareLists(lRest, rRest)
              case r => r
            }
          case (l: Int, r: Int) =>
            // Both items are Ints... compare
            if (l < r) {
              Result.Ordered
            } else if (r < l) {
              Result.Unordered
            } else {
              // Not sure.. need to keep looking
              compareLists(lRest, rRest)
            }
          case (l: Int, r @ ContentList(_)) =>
            // Left side is an int, but right side is list.. convert
            compareLists(ContentList(Seq(l)) +: lRest, right)
          case (l @ ContentList(_), r: Int) =>
            // Left side is a list, but right side is an int .. convert
            compareLists(left, ContentList(Seq(rH)) +: rRest)
        }
    }

  private def parse(
      lines: Seq[String]
  ): Seq[ListPairs] = lines
    .grouped(3)
    .map { case (first :: second :: _) =>
      val f = constructContent(first, Seq.empty, None).head
      val s = constructContent(second, Seq.empty, None).head
      (f, s)
    }
    .toSeq

  private def constructContent(
      line: String,
      acc: Seq[Content],
      bracketAcc: Option[String]
  ): Seq[Content] = {
    if (line.isEmpty) {
      acc
    } else {
      val head = line.charAt(0)
      val rest = line.drop(1)
      head match {
        case '[' if bracketAcc.isEmpty =>
          constructContent(rest, acc, Some(""))
        case ']'
            if bracketAcc.nonEmpty &&
              bracketAcc
                .map(_.count(_ == '[')) == bracketAcc.map(_.count(_ == ']')) =>
          val bracketVal = constructContent(bracketAcc.get, Seq.empty, None)
          constructContent(rest, acc :+ ContentList(bracketVal), None)
        case c if bracketAcc.nonEmpty =>
          constructContent(rest, acc, bracketAcc.map(_ :+ c))
        case ',' =>
          constructContent(rest, acc, bracketAcc)
        case _ =>
          val substr = line.takeWhile(_.isDigit)
          constructContent(
            line.drop(substr.length),
            acc :+ substr.toInt,
            bracketAcc
          )
      }
    }
  }

  private enum Result:
    case Ordered, Unordered, Unknown

  type ListPairs = (Content, Content)
  type Content = ContentList | Int
  case class ContentList(contents: Seq[Content])
}
