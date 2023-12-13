package puzzles
import main.Puzzle
import scala.annotation.tailrec
import scala.annotation.newMain

object PuzzleEleven extends Puzzle {
  type Decision = Long => Long
  type Operation = Long => Long
  override def partOne(fileLines: Seq[String]): Unit = {
    val monkeys = parse(fileLines)
    println(
      executeRounds(monkeys, 20, v => v / 3)
        .sorted(Ordering.Long.reverse)
        .take(2)
        .reduce(_ * _)
    )
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val monkeys = parse(fileLines)
    val baseModulo = monkeys.map(_.test_num).reduce(_ * _)

    println(
      executeRounds(monkeys, 10_000, v => v % baseModulo)
        .sorted(Ordering.Long.reverse)
        .take(2)
        .reduce(_ * _)
    )
  }

  private def executeRounds(
      monkeys: Seq[Monkey],
      numRounds: Int,
      worryModifier: Long => Long
  ): Seq[Long] = {
    (0 to numRounds - 1)
      .foldLeft(monkeys, Map.empty[Int, Long]) {
        case ((monkeys, inspects), index) =>
          val (newMonkeys, newInspects) =
            executeRound(monkeys, worryModifier)

          (
            newMonkeys,
            inspects ++ newInspects.map { case (k, v) =>
              k -> (v + inspects.getOrElse(k, 0L))
            }
          )
      }
      ._2
      .toSeq
      .map(_._2)
  }

  private def executeRound(
      monkeys: Seq[Monkey],
      modifier: Long => Long
  ): (Seq[Monkey], Map[Int, Long]) = {
    monkeys.indices.foldLeft((monkeys, Map.empty[Int, Long])) {
      case ((ms, inspections), mIdx) =>
        val monkey = ms(mIdx)
        val itemMovements = monkey.items
          .map { item =>
            val newItemWorry = modifier(monkey.operation(item))
            (newItemWorry, monkey.decision(newItemWorry))
          }
          .groupMap(_._2)(_._1)

        // Move all items to target monkey(s)
        val newMs = ms.map {
          case m @ Monkey(id, _, _, _, _) if id == monkey.id =>
            m.copy(items = Seq.empty)
          case monkey =>
            val newItems = itemMovements.getOrElse(monkey.id, Seq.empty)
            monkey.copy(items = monkey.items ++ newItems)
        }

        val newInspections =
          inspections + (monkey.id -> monkey.items.length.toLong)
        (newMs, newInspections)
    }
  }

  private def parse(lines: Seq[String]): Seq[Monkey] = {
    lines match {
      case Nil => Seq.empty
      case ls =>
        val monkeyLines = ls.takeWhile(!_.isEmpty).map(_.trim)
        // Items, Operation multiplier, Test division int, true monkey int, false monkey int
        val a = monkeyLines
          .foldLeft((Seq.empty[Long], identity[Long], 0, 0, 0, -1)) {
            /*
              Monkey 0:
              Starting items: 85, 77, 77
              Operation: new = old * 7
              Test: divisible by 19
                  If true: throw to monkey 6
                  If false: throw to monkey 7
             */
            case (acc, s"Monkey $id:") =>
              acc.copy(_6 = id.toInt)
            case (acc, s"Starting items: $items") =>
              acc.copy(_1 = items.split(", ").map(_.toLong))
            case (acc, s"Operation: new = old * old") =>
              val op: Operation = old => old * old
              acc.copy(_2 = op)
            case (acc, s"Operation: new = old * $v") =>
              val op: Operation = old => old * v.toInt
              acc.copy(_2 = op)
            case (acc, s"Operation: new = old + $v") =>
              val op: Operation = old => old + v.toInt
              acc.copy(_2 = op)
            case (acc, s"Test: divisible by $v") =>
              acc.copy(_3 = v.toInt)
            case (acc, s"If true: throw to monkey $v") =>
              acc.copy(_4 = v.toInt)
            case (acc, s"If false: throw to monkey $v") =>
              acc.copy(_5 = v.toInt)
            case (acc, v) =>
              println(s"[WARN] Unknown line '$v'")
              acc
          }

        val monkey = Monkey(
          a._6,
          a._1,
          a._2,
          v => if (v % a._3 == 0) a._4 else a._5,
          a._3
        )
        monkey +: parse(ls.drop(monkeyLines.length + 1))
    }
  }

  case class Monkey(
      id: Int,
      items: Seq[Long],
      operation: Operation,
      decision: Decision,
      test_num: Long
  )
}
