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
    assertEquals(Field.from(smallInput1), Some(expected))
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
    assertEquals(Field.from(smallSimplifiedInput1), Some(expected))
  }

object Day10Suite:

//  val bigInput: List[String] = getLinesFromFile("src/test/scala/day10_input.txt")

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
