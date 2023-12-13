package puzzles

import main.Puzzle

object PuzzleThirteen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    println(findOrderedPairs(parse(fileLines)).sum)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    import ContentList.wrap // :eyes:

    val flattenedLines = parse(fileLines).flatMap(a => Seq(a._1, a._2))
    val dividers = Seq(2, 6).map(x => wrap(wrap(x)))

    val sorted = sort(flattenedLines ++ dividers)
    val dividerIndex = dividers
      .map(dIndex => sorted.indexOf(dIndex) + 1)
      .reduce(_ * _)
    println(dividerIndex)
  }

  private def sort(content: Seq[Content]): Seq[Content] =
    content.sortWith((a, b) =>
      compareLists(Seq(a), Seq(b)) == ComparisonResult.Ordered
    )

  private def findOrderedPairs(pairs: Seq[ContentPair]): Seq[Int] = {
    pairs.zipWithIndex.collect {
      case ((ContentList(l), ContentList(r)), index) =>
        compareLists(l, r) match
          case ComparisonResult.Ordered => Some(index + 1)
          case _                        => None
    }.flatten
  }

  private def compareLists(
      left: Seq[Content],
      right: Seq[Content]
  ): ComparisonResult = (left, right) match {
    case (Nil, Nil) => ComparisonResult.Unknown
    case (Nil, _)   => ComparisonResult.Ordered
    case (_, Nil)   => ComparisonResult.Unordered
    case (leftHead :: leftRest, rightHead :: rightRest) =>
      (leftHead, rightHead) match {
        case (l: Int, r: Int) if l < r    => ComparisonResult.Ordered
        case (l: Int, r: Int) if r < l    => ComparisonResult.Unordered
        case (_: Int, _: Int)             => compareLists(leftRest, rightRest)
        case (_: Int, r @ ContentList(_)) =>
          // Type mismatch. Box leftHead in a ContentList and retry
          compareLists(ContentList(Seq(leftHead)) +: leftRest, right)
        case (l @ ContentList(_), _: Int) =>
          // Type mismatch. Box rightHead in a ContentList and retry
          compareLists(left, ContentList(Seq(rightHead)) +: rightRest)
        case (ContentList(l), ContentList(r)) =>
          compareLists(l, r) match
            case ComparisonResult.Unknown => compareLists(leftRest, rightRest)
            case r                        => r
      }
  }

  private def parse(lines: Seq[String]): Seq[ContentPair] = lines
    .grouped(3)
    .map { case (first :: second :: _) =>
      (
        constructContent(first, Seq.empty, None).head,
        constructContent(second, Seq.empty, None).head
      )
    }
    .toSeq

  private def constructContent(
      line: String,
      acc: Seq[Content],
      nestedAcc: Option[String]
  ): Seq[Content] =
    if (line.isEmpty) acc
    else {
      val tail = line.drop(1)
      val canCloseNested =
        nestedAcc.map(n => n.count(_ == '[') == n.count(_ == ']'))

      line.head match {
        // Nested handling
        case '[' if nestedAcc.isEmpty =>
          constructContent(tail, acc, Some(""))
        case ']' if canCloseNested == Some(true) =>
          val nestedValue = constructContent(nestedAcc.get, Seq.empty, None)
          constructContent(tail, acc :+ ContentList(nestedValue), None)
        case c if nestedAcc.nonEmpty =>
          constructContent(tail, acc, nestedAcc.map(_ :+ c))

        // Top level handling
        case ',' =>
          constructContent(tail, acc, nestedAcc)
        case _ =>
          val substr = line.takeWhile(_.isDigit)
          constructContent(
            line.drop(substr.length),
            acc :+ substr.toInt,
            nestedAcc
          )
      }
    }

  private enum ComparisonResult:
    case Ordered, Unordered, Unknown

  private type Content = ContentList | Int
  private type ContentPair = (Content, Content)

  private case class ContentList(contents: Seq[Content])
  private object ContentList {
    def wrap = (c: Content) => ContentList(Seq(c))
  }
}
