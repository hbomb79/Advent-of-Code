package puzzles

import main.Puzzle
import scala.annotation.tailrec
import scala.collection.mutable.{Map, HashMap, ArrayBuffer}

object PuzzleSixteen extends Puzzle {

  override def partOne(fileLines: Seq[String]): Unit = {
    val solver = DfsSolver(fileLines)
    val pressure =
      solver.memoDfs(
        solver.allValvesMapped("AA"),
        solver.allValves.filter(_.flowRate > 0).map(_.label).toList,
        30
      )

    println(pressure)
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    val solver = DfsSolver(fileLines)
    val pressure =
      solver.dfsWithElephant(
        solver.allValvesMapped("AA"),
        solver.allValves.filter(_.flowRate > 0).map(_.label).toList,
        26
      )

    println(pressure)
  }

  private class DfsSolver(lines: Seq[String]) {
    type MemoKey = (Valve, List[String], Int)
    val Memo: HashMap[MemoKey, Int] = HashMap.empty

    val allValves = parse(lines)
    val allValvesMapped = allValves.map(v => v.label -> v).toMap

    val edges = {
      val valveIndices =
        allValves.zipWithIndex.map((v, i) => v.label -> i).toMap

      Map.from(
        allValves.map(v =>
          valveIndices(v.label) -> v.leadsTo.map(valveIndices).toSet
        )
      )
    }

    val valveCosts = IterHelper
      .allPairsShortestPath(allValves.indices.toSet, edges)
      .map((idx, links) =>
        allValves(idx).label -> links.map((idx, path) =>
          allValves(idx).label -> (path.length - 1)
        )
      )

    def memoDfs(
        currentValve: Valve,
        targetValves: List[String],
        time: Int
    ) = Memo.getOrElseUpdate(
      (currentValve, targetValves, time),
      dfs(currentValve, targetValves, time)
    )

    private def dfs(
        currentValve: Valve,
        targetValves: List[String],
        time: Int
    ): Int = {
      val results = for {
        (t, ts) <- IterHelper.elementExtractCombinations(targetValves)
        cost = valveCosts(currentValve.label)(t)
        if cost < time
      } yield {
        val r = memoDfs(allValvesMapped(t), ts, time - cost - 1)
        allValvesMapped(t).flowRate * (time - cost - 1) + r
      }
      results.maxOption.getOrElse(0)
    }

    def dfsWithElephant(
        currentValve: Valve,
        targetValves: List[String],
        time: Int
    ): Int = {
      val results = for {
        (t, ts) <- IterHelper.elementExtractCombinations(targetValves)
        cost = valveCosts(currentValve.label)(t)
        if cost < time
      } yield {
        val r = dfsWithElephant(allValvesMapped(t), ts, time - cost - 1)
        allValvesMapped(t).flowRate * (time - cost - 1) + r
      }

      val myResult = memoDfs(allValvesMapped("AA"), targetValves, 26)
      (results ++ Seq(myResult)).max
    }
  }

  private def parse(lines: Seq[String]): Seq[Valve] = lines.collect {
    case s"Valve $label has flow rate=$flowRate; tunnel$_ lead$_ to valve$_ $valves" =>
      Valve(label, flowRate.toInt, valves.split(", "))
  }

  case class Valve(label: String, flowRate: Int, leadsTo: Seq[String])
}
