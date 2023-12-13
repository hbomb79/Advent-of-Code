package puzzles

import main.Puzzle
import PuzzleSeventeen.Shape
import scala.annotation.tailrec

object PuzzleSeventeen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val game = Game(fileLines.head, 7)
    val blocks = game.simulate(2022, None, Seq.empty)
    println(blocks.map(_.y).max - 1)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    print("\nGenerating data...")
    val N = 1_000_000_000_000L
    val game = Game(fileLines.head, 7)
    val maxHeightStates = (1 to 4000)
      .foldLeft(Seq.empty[Shape], Seq.empty[Long]) {
        case ((accShapes, accMaxHeights), i) =>
          if (i % 1000 == 0) {
            print(s" ${i / 1000 * 25}%...")
          }
          val newAcc = game.simulate(i, None, accShapes)
          val newMax =
            Math.max(accMaxHeights.lastOption.getOrElse(0L), newAcc.last.y)
          (newAcc, accMaxHeights :+ newMax)
      }
      ._2
    println

    val heightDeltas = maxHeightStates
      .sliding(2)
      .map { case Seq(prev, current) =>
        current - prev
      }

    val (startOfCycle, cycleLength) = findCycleInList(heightDeltas.toVector)
    val maxHeightBeforeCycle = maxHeightStates(startOfCycle) - 1
    val numberOfCycles = (N - startOfCycle) / cycleLength
    val cycleRemainder = (N - startOfCycle) % cycleLength

    val cycleDeltaHeight =
      (maxHeightStates(startOfCycle + cycleLength) - 1) - maxHeightBeforeCycle

    // Our N doesn't cleanly divide in to our cycle size
    // so we need to manually tack this on at the end
    val partialCycleDeltaHeight =
      (maxHeightStates(startOfCycle + cycleRemainder.toInt) - 1)
        - maxHeightBeforeCycle

    val result =
      maxHeightBeforeCycle + (cycleDeltaHeight * numberOfCycles) + partialCycleDeltaHeight - 1
    println(result)
  }

  private def findCycleInList(
      list: Vector[Long]
  ): (Int, Int) = {
    def search(i: Int): (Int, Int) = {
      val key = list.slice(i, i + 50)
      val keyIndex = list.indexOfSlice(key)
      val cycle = list.slice(keyIndex, i)
      if keyIndex < i && list.drop(keyIndex).startsWith(cycle) then
        keyIndex -> cycle.size
      else search(i + 1)
    }

    search(0)
  }

  class Game(forceSource: String, width: Int) {
    val ForceGen = forceGenerator(forceSource).iterator
    val ShapeGen = shapeGenerator(
      Seq(
        HLine,
        Cross,
        Corner,
        VLine,
        Square
      )
    ).iterator

    val WorldBoundaries = Seq(
      BoundingBox(-1, 0, 1, Integer.MAX_VALUE), // Left edge
      BoundingBox(width, 0, 1, Integer.MAX_VALUE), // Right edge
      BoundingBox(0, 0, width, 1) // Bottom
    )

    @tailrec
    final def simulate(
        limit: Long,
        fallingShape: Option[Shape],
        settledShapes: Seq[Shape]
    ): Seq[Shape] = fallingShape match {
      case None if settledShapes.size == limit =>
        settledShapes
      case None =>
        val spawnY = findHighestShape(settledShapes).map(_.y).getOrElse(1L) + 3
        val shape = ShapeGen.next()(2, spawnY)
        val nShape = shape.copy(y = shape.y + shape.height)
        simulate(
          limit,
          Some(nShape),
          settledShapes
        )
      case Some(shape) =>
        assert(
          shape.y > 0 && shape.x >= 0,
          s"Shape ${shape} has left the world boundary"
        )

        val force = ForceGen.next
        val (forceMovedShape) = force match {
          case '<' => shape.copy(x = shape.x - 1)
          case '>' => shape.copy(x = shape.x + 1)
        }

        val shape2 =
          if isShapeColliding(forceMovedShape, settledShapes) then shape
          else forceMovedShape

        val gravityMovedShape = shape2.copy(y = shape2.y - 1)
        if isShapeColliding(gravityMovedShape, settledShapes) then
          simulate(limit, None, settledShapes :+ shape2)
        else simulate(limit, Some(gravityMovedShape), settledShapes)
    }

    private def findHighestShape(shapes: Seq[Shape]): Option[Shape] =
      shapes.maxByOption(_.y)

    private def isShapeColliding(
        shape: Shape,
        settledShapes: Seq[Shape]
    ): Boolean = {
      val allBoundaries = settledShapes.flatMap(_.absoluteBoundingBoxes)
        ++ WorldBoundaries

      shape.absoluteBoundingBoxes
        .find(shapeBound =>
          allBoundaries.find(_.isColliding(shapeBound)).nonEmpty
        )
        .nonEmpty
    }
  }

  sealed case class BoundingBox(x: Long, y: Long, width: Long, height: Long) {
    def right: Long = x + width
    def bottom: Long = y + height

    def isColliding(b: BoundingBox): Boolean =
      x < b.right && right > b.x && y < b.bottom && bottom > b.y
  }

  sealed case class Shape(
      val x: Long,
      val y: Long,
      val boundingBoxes: Seq[BoundingBox]
  ) {
    def height = boundingBoxes.map(_.bottom).max
    def absoluteBoundingBoxes =
      boundingBoxes.map(b => b.copy(x = x + b.x, y = y - (b.y + b.height)))
  }

  private def shapeGenerator(
      generators: Seq[ShapeGenerator]
  ): Iterable[(Long, Long) => Shape] =
    LazyList
      .from(0)
      .map(i =>
        (x, y) => Shape(x, y, generators(i % generators.length).boundingBoxes)
      )

  private def forceGenerator(
      forces: String
  ): Iterable[Char] =
    LazyList
      .from(0)
      .map(i => forces.charAt(i % forces.length))

  trait ShapeGenerator {
    def boundingBoxes: Seq[BoundingBox]
  }
  object HLine extends ShapeGenerator {
    override def boundingBoxes: Seq[BoundingBox] = Seq(
      BoundingBox(0, 0, 4, 1)
    )
  }
  object VLine extends ShapeGenerator {
    def boundingBoxes: Seq[BoundingBox] = Seq(
      BoundingBox(0, 0, 1, 4)
    )
  }
  object Cross extends ShapeGenerator {
    override def boundingBoxes: Seq[BoundingBox] = Seq(
      BoundingBox(0, 1, 3, 1),
      BoundingBox(1, 0, 1, 3)
    )
  }
  object Square extends ShapeGenerator {
    override def boundingBoxes: Seq[BoundingBox] = Seq(
      BoundingBox(0, 0, 2, 2)
    )
  }
  object Corner extends ShapeGenerator {
    override def boundingBoxes: Seq[BoundingBox] = Seq(
      BoundingBox(0, 2, 3, 1),
      BoundingBox(2, 0, 1, 3)
    )
  }
}
