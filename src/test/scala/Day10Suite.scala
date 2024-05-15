import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import Day10.*
import Day10.PipeType.*
import Day10.Tile.*
import Day10Suite.*

class Day10Suite extends ScalaCheckSuite:

  test("parsing small input 1") {
    val expected = Field(rows =
      Vector(
        Vector(Horizontal, NorthAndEast, Vertical, SouthAndEast, SouthAndWest).map(Pipe.apply),
        Vector(Pipe(SouthAndWest), Start, Pipe(Horizontal), Pipe(SouthAndWest), Pipe(Vertical)),
        Vector(NorthAndEast, Vertical, SouthAndWest, Vertical, Vertical).map(Pipe.apply),
        Vector(Horizontal, NorthAndEast, Horizontal, NorthAndWest, Vertical).map(Pipe.apply),
        Vector(NorthAndEast, Vertical, Horizontal, NorthAndWest, SouthAndEast).map(Pipe.apply)
      )
    )
    assertEquals(Field.parse(smallInput1), Some(expected))
  }

  test("parsing small simplified input 1") {
    val expected = Field(rows =
      Vector(
        Vector.fill(5)(Ground),
        Vector(Ground, Start, Pipe(Horizontal), Pipe(SouthAndWest), Ground),
        Vector(Ground, Pipe(Vertical), Ground, Pipe(Vertical), Ground),
        Vector(Ground, Pipe(NorthAndEast), Pipe(Horizontal), Pipe(NorthAndWest), Ground),
        Vector.fill(5)(Ground)
      )
    )
    assertEquals(Field.parse(smallSimplifiedInput1), Some(expected))
  }

  test("start position for small input 1 is Pos(row = 1, col = 1)") {
    assertEquals(Field.parse(smallInput1).flatMap(_.getStartPos), Some(Pos(row = 1, col = 1)))
  }

  test("start position for small simplified input 1 is Pos(row = 1, col = 1)") {
    assertEquals(
      Field.parse(smallSimplifiedInput1).flatMap(_.getStartPos),
      Some(Pos(row = 1, col = 1))
    )
  }

  test("start position for small input 2 is Pos(row = 2, col = 0)") {
    assertEquals(Field.parse(smallInput2).flatMap(_.getStartPos), Some(Pos(row = 2, col = 0)))
  }

  test("start position for small simplified input 2 is Pos(row = 2, col = 0)") {
    assertEquals(
      Field.parse(smallSimplifiedInput2).flatMap(_.getStartPos),
      Some(Pos(row = 2, col = 0))
    )
  }

  test("start position for big input is defined") {
    assert(Field.parse(bigInput).flatMap(_.getStartPos).isDefined)
  }

  test("one-step moves from start found for small input 1") {
    assertEquals(
      for {
        field <- Field.parse(smallInput1)
        start <- field.getStartPos
      } yield field.oneStepFrom(start),
      Some(Set((Pipe(Horizontal), Pos(row = 1, col = 2)), (Pipe(Vertical), Pos(row = 2, col = 1))))
    )
  }

  test("one-step moves from start found for small simplified input 1") {
    assertEquals(
      for {
        field <- Field.parse(smallSimplifiedInput1)
        start <- field.getStartPos
      } yield field.oneStepFrom(start),
      Some(Set((Pipe(Horizontal), Pos(row = 1, col = 2)), (Pipe(Vertical), Pos(row = 2, col = 1))))
    )
  }

  test("one-step moves from start found for small input 2") {
    assertEquals(
      for {
        field <- Field.parse(smallInput2)
        start <- field.getStartPos
      } yield field.oneStepFrom(start),
      Some(Set((Pipe(NorthAndWest), Pos(row = 2, col = 1)), (Pipe(Vertical), Pos(row = 3, col = 0))))
    )
  }

  test("one-step moves from start found for small simplified input 2") {
    assertEquals(
      for {
        field <- Field.parse(smallSimplifiedInput2)
        start <- field.getStartPos
      } yield field.oneStepFrom(start),
      Some(Set((Pipe(NorthAndWest), Pos(row = 2, col = 1)), (Pipe(Vertical), Pos(row = 3, col = 0))))
    )
  }

  test("#2 one-step moves from start found for big input") {
    assertEquals(
      for {
        field <- Field.parse(bigInput)
        start <- field.getStartPos
      } yield field.oneStepFrom(start).size,
      Some(2)
    )
  }

  test("loop for small input 1") {
    assertEquals(
      Field.parse(smallInput1).flatMap(_.getLoop),
      Some(
        Loop(
          firstPath = NonEmptyList.of(Pos(1, 1), Pos(2, 1), Pos(3, 1), Pos(3, 2), Pos(3, 3)),
          secondPath = NonEmptyList.of(Pos(1, 1), Pos(1, 2), Pos(1, 3), Pos(2, 3), Pos(3, 3))
        )
      )
    )
  }

  test("loop for small input 1 is same of small simplified input 1") {
    assertEquals(
      Field.parse(smallInput1).flatMap(_.getLoop),
      Field.parse(smallSimplifiedInput1).flatMap(_.getLoop)
    )
  }

  test("loop for small input 2") {
    assertEquals(
      Field.parse(smallInput2).flatMap(_.getLoop),
      Some(
        Loop(
          firstPath = NonEmptyList
            .of((2, 0), (3, 0), (4, 0), (4, 1), (3, 1), (3, 2), (3, 3), (3, 4), (2, 4))
            .map(Pos.apply.tupled),
          secondPath = NonEmptyList
            .of((2, 0), (2, 1), (1, 1), (1, 2), (0, 2), (0, 3), (1, 3), (2, 3), (2, 4))
            .map(Pos.apply.tupled)
        )
      )
    )
  }

  test("loop for small input 2 is same of small simplified input 2") {
    assertEquals(
      Field.parse(smallInput2).flatMap(_.getLoop),
      Field.parse(smallSimplifiedInput2).flatMap(_.getLoop)
    )
  }

  test("steps count to farthest point in loop is 4 for small input 1") {
    assertEquals(stepsCountToFarthestInLoop(smallInput1), Some(4))
  }

  test("steps count to farthest point in loop is same for small input 1 and simplified one") {
    assertEquals(
      stepsCountToFarthestInLoop(smallInput1),
      stepsCountToFarthestInLoop(smallSimplifiedInput1)
    )
  }

  test("steps count to farthest point in loop is 8 for small input 2") {
    assertEquals(stepsCountToFarthestInLoop(smallInput2), Some(8))
  }

  test("steps count to farthest point in loop is same for small input 2 and simplified one") {
    assertEquals(
      stepsCountToFarthestInLoop(smallInput2),
      stepsCountToFarthestInLoop(smallSimplifiedInput2)
    )
  }

  test("loop branches has same length for big input") {
    assertEquals(
      Field.parse(bigInput).flatMap(_.getLoop.map(l => l.firstPath.length == l.secondPath.length)),
      Some(true)
    )
  }

  test("steps count to farthest point in loop is 6_828 for big input") {
    assertEquals(stepsCountToFarthestInLoop(bigInput), Some(6_828))
  }

object Day10Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day10_input.txt")

  val smallInput1: List[String] = List(
    "-L|F7",
    "7S-7|",
    "L|7||",
    "-L-J|",
    "L|-JF"
  )
  val smallSimplifiedInput1: List[String] = List(
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  )

  val smallInput2: List[String] = List(
    "7-F7-",
    ".FJ|7",
    "SJLL7",
    "|F--J",
    "LJ.LJ"
  )
  val smallSimplifiedInput2: List[String] = List(
    "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  )
