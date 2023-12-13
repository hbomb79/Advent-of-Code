package puzzles

import main.Puzzle

object PuzzleTen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val outs = executeCycles(
      extractInstructions(fileLines),
      (cycle, reg) => {
        val rcBased = cycle - 20
        if (rcBased == 0 || rcBased % 40 == 0) Seq(cycle * reg)
        else Seq.empty
      }
    )
    println(outs.sum)
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    val outs = executeCycles[Char](
      extractInstructions(fileLines),
      (cycle, reg) => {
        val spriteRc = (cycle % 40) - 1
        if ((spriteRc - 1 to spriteRc + 1).contains(reg)) Seq('#')
        else Seq('.')
      }
    )
    println
    outs.grouped(40).map(_.mkString).toSeq.map(println)
  }

  private def extractInstructions(lines: Seq[String]): Seq[CycleInstruction] = {
    lines.flatMap[CycleInstruction] {
      case "noop"     => Seq(CycleNoOp)
      case s"addx $x" => Seq(CycleNoOp, CycleAddReg(x.toInt))
    }
  }

  private def executeCycles[A](
      cycles: Seq[CycleInstruction],
      generator: (cycle: Int, register: Int) => Seq[A]
  ): Seq[A] = {
    cycles.zipWithIndex
      .foldLeft(1, Seq.empty[A]) { case ((reg, acc), (instr, cycle)) =>
        val newAcc = acc ++ generator(cycle + 1, reg)
        val newReg = instr match {
          case CycleNoOp      => reg
          case CycleAddReg(n) => reg + n
        }

        (newReg, newAcc)
      }
      ._2
  }

  sealed trait CycleInstruction
  case object CycleNoOp extends CycleInstruction
  case class CycleAddReg(n: Int) extends CycleInstruction
}
