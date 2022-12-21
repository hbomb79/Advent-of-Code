package puzzles

import main.Puzzle
import scala.annotation.tailrec
import scala.annotation.newMain

object PuzzleTwentyOne extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val ms = monkeys(fileLines).map(m => m.name -> m).toMap
    println(calculate(ms, 0)("root")._2)
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    val ms = monkeys(fileLines).map(m => m.name -> m).toMap
    val newJob = ms("root").job match
      case Wait(monkeyA, monkeyB, _) => TestEq(monkeyA, monkeyB)
      case _                         => throw new IllegalStateException

    val resolvedMonkeys = calculate(
      ms.updated("root", ms("root").copy(job = newJob))
        .updated("humn", ms("humn").copy(job = Idle)),
      0
    )

    val result =
      reverseCalculate(resolvedMonkeys("root"), "humn", resolvedMonkeys)
    println(result)
  }

  private def reverseCalculate(
      rootMonkey: Monkey,
      target: String,
      all: Map[String, Monkey]
  ): Long = {
    def calculate(monkey: Monkey, value: Long): Long = {
      monkey match {
        case Monkey(name, _) if name == target => value
        case Monkey(_, Wait(monkeyA, monkeyB, operation)) =>
          (all(monkeyA).job, all(monkeyB).job) match {
            case (Yell(number), _) =>
              calculate(
                all(monkeyB),
                operation match
                  case Operation.Div | Operation.Sub =>
                    runOperation(operation, number, value)
                  case _ => runOperation(operation.inverse, value, number)
              )
            case (_, Yell(number)) =>
              calculate(
                all(monkeyA),
                runOperation(operation.inverse, value, number)
              )
            case _ => throw new IllegalStateException
          }
        case m => throw new IllegalStateException
      }
    }

    rootMonkey match {
      case Monkey(_, TestEq(a, b)) =>
        (all(a), all(b)) match {
          case (Monkey(_, Yell(known)), m) => calculate(m, known)
          case (m, Monkey(_, Yell(known))) => calculate(m, known)
          case _                           => throw new IllegalStateException
        }
      case _ =>
        throw new IllegalStateException("Invalid intial monkey, not a root!")
    }
  }

  @tailrec
  private def calculate(
      monkeys: Map[String, Monkey],
      lastWaitingSize: Int
  ): Map[String, Monkey] = {
    val (waiting, finished) =
      monkeys.partition((a, b) => b.job.isInstanceOf[Wait])
    if (waiting.size == 0 || waiting.size == lastWaitingSize) then monkeys
    else
      val newState = waiting.foldLeft(monkeys) { case (acc, (name, monkey)) =>
        acc.updated(name, monkey.next(acc))
      }

      calculate(newState, waiting.size)
  }

  def monkeys(lines: Seq[String]): Seq[Monkey] = lines.map {
    case s"$name: $num" if num.toIntOption.nonEmpty =>
      Monkey(name, Yell(num.toLong))
    case s"$name: $a $op $b" =>
      val operation = op match {
        case "/" => Operation.Div
        case "*" => Operation.Mult
        case "-" => Operation.Sub
        case "+" => Operation.Add
      }

      Monkey(name, Wait(a, b, operation))
  }

  private def runOperation(op: Operation, aNum: Long, bNum: Long): Long =
    op match {
      case Operation.Div  => aNum / bNum
      case Operation.Sub  => aNum - bNum
      case Operation.Add  => aNum + bNum
      case Operation.Mult => aNum * bNum
    }

  enum Operation {
    case Div, Sub, Add, Mult

    def inverse: Operation = this match {
      case Operation.Add  => Operation.Sub
      case Operation.Div  => Operation.Mult
      case Operation.Sub  => Operation.Add
      case Operation.Mult => Operation.Div
    }
  }

  case class State(monkeys: Map[String, Monkey])

  sealed trait Job
  case class Yell(number: Long) extends Job
  case object Idle extends Job
  case class TestEq(monkeyA: String, monkeyB: String) extends Job
  case class Wait(monkeyA: String, monkeyB: String, operation: Operation)
      extends Job

  case class Monkey(name: String, job: Job) {
    def next(monkeys: Map[String, Monkey]): Monkey = job match
      case Wait(a, b, operation) =>
        (monkeys(a).job, monkeys(b).job) match {
          case (Yell(aNumStr), Yell(bNumStr)) =>
            val aNum = aNumStr.toLong
            val bNum = bNumStr.toLong
            copy(job = Yell(runOperation(operation, aNum, bNum)))
          case _ => this
        }
      case _ => this
  }
}
