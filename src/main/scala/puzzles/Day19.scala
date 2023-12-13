package puzzles

import main.Puzzle
import IterHelper._

object PuzzleNineteen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val result = blueprints(fileLines)
      .map(State(Materials(0, 0, 0, 0), Robots(1, 0, 0, 0), _, 24))
      .zipWithIndex
      .map { (state, i) =>
        search(state).map(_.mats.geode).max * (i + 1)
      }
      .sum

    println(result)
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    val result = blueprints(fileLines)
      .take(3)
      .map(State(Materials(0, 0, 0, 0), Robots(1, 0, 0, 0), _, 32))
      .map(search(_).map(_.mats.geode).max)
      .product

    println(result)
  }

  case class State(mats: Materials, bots: Robots, bp: Blueprint, time: Int) {
    def collect(n: Int): State = copy(
      time = time - n,
      mats = mats.copy(
        ore = (mats.ore + bots.ore * n),
        clay = (mats.clay + bots.clay * n),
        obs = (mats.obs + bots.obs * n),
        geode = mats.geode + bots.geode * n
      )
    )

    def buildOreBot: State = copy(
      mats.copy(ore = mats.ore - bp.oreBotOreCost),
      bots.copy(ore = bots.ore + 1)
    )

    def queueOreBot: Option[State] =
      lazy val missingOre = (bp.oreBotOreCost - mats.ore).max(0)
      lazy val turnsNeeded = (missingOre + bots.ore - 1) / bots.ore
      Option.when(bots.ore < bp.maxOreWeEverWant && turnsNeeded + 1 < time) {
        collect(turnsNeeded).collect(1).buildOreBot
      }

    def buildClayBot: State = copy(
      mats.copy(ore = mats.ore - bp.clayBotOreCost),
      bots.copy(clay = bots.clay + 1)
    )

    def queueClayBot: Option[State] =
      lazy val missingOre = (bp.clayBotOreCost - mats.ore).max(0)
      lazy val turnsNeeded = (missingOre + bots.ore - 1) / bots.ore
      Option.when(bots.clay < bp.clayCost && turnsNeeded + 1 < time) {
        collect(turnsNeeded).collect(1).buildClayBot
      }

    def buildObsBot: State = copy(
      mats.copy(
        ore = mats.ore - bp.obsBotOreCost,
        clay = mats.clay - bp.clayCost
      ),
      bots.copy(obs = bots.obs + 1)
    )

    def queueObs: Option[State] = {
      lazy val missingOre = (bp.obsBotOreCost - mats.ore).max(0)
      lazy val turnsNeededO = (missingOre + bots.ore - 1) / bots.ore
      lazy val missingClay = (bp.clayCost - mats.clay).max(0)
      lazy val turnsNeededC = (missingClay + bots.clay - 1) / bots.clay
      lazy val turnsNeeded = turnsNeededO max turnsNeededC

      Option.when(
        bots.obs < bp.obsCost && bots.clay >= 1 && turnsNeeded + 1 < time
      ) {
        collect(turnsNeeded).collect(1).buildObsBot
      }
    }

    def buildGeodeBot = copy(
      mats.copy(
        ore = mats.ore - bp.geodeBotOreCost,
        obs = mats.obs - bp.obsCost
      ),
      bots.copy(geode = bots.geode + 1)
    )

    def queueGeode: Option[State] = {
      lazy val missingOre = (bp.geodeBotOreCost - mats.ore).max(0)
      lazy val turnsNeededO = (missingOre + bots.ore - 1) / bots.ore
      lazy val missingObs = (bp.obsCost - mats.obs).max(0)
      lazy val turnsNeededB = (missingObs + bots.obs - 1) / bots.obs
      lazy val turnsNeeded = turnsNeededO max turnsNeededB

      Option.when(bots.obs >= 1 && turnsNeeded + 1 < time) {
        collect(turnsNeeded).collect(1).buildGeodeBot
      }
    }

    def next: List[State] =
      if time <= 0 then List(this)
      else
        List(queueOreBot, queueClayBot, queueObs, queueGeode).flatten match {
          case Nil  => List(this.collect(time))
          case bots => bots
        }
  }

  case class Materials(ore: Int, clay: Int, obs: Int, geode: Int):
    def nonNegative = (ore min clay min obs min geode) >= 0

  case class Robots(ore: Int, clay: Int, obs: Int, geode: Int)

  case class Blueprint(
      oreBotOreCost: Int,
      clayBotOreCost: Int,
      obsBotOreCost: Int,
      clayCost: Int,
      geodeBotOreCost: Int,
      obsCost: Int
  ) {
    val maxOreWeEverWant =
      obsBotOreCost max clayBotOreCost max geodeBotOreCost max oreBotOreCost
  }

  def blueprints(input: Seq[String]) = input.map {
    case s"$_ costs $oo ore.$_ costs $co ore.$_ costs $bo ore and $bc clay.$_ costs $go ore and $gb obsidian." =>
      Blueprint(oo.toInt, co.toInt, bo.toInt, bc.toInt, go.toInt, gb.toInt)
  }

  // TODO Refactor to FP
  def search(start: State) = {
    import collection.mutable.{Set, Map, Queue}

    val q = Queue(start)
    val finalStates = Set.empty[State]
    while q.nonEmpty do
      val state = q.dequeue()
      val (finished, continuing) = state.next.partition(_.time <= 0)
      finalStates ++= finished
      q.enqueueAll(continuing)

    finalStates.toSet
  }

  def sort(states: Seq[State]) =
    states.sortBy(s => (-s.mats.geode, -s.mats.obs, -s.mats.clay, -s.mats.ore))
}
