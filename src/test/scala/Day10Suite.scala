import Day10.*
import Day10.CardinalDirection.*
import Day10.Inversion.*
import Day10.PipeType.*
import Day10.Tile.*
import Day10Suite.*
import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day10Suite extends ScalaCheckSuite:

  test("parsing smallInput1"):
    val expected = Field(rows =
      Vector(
        Vector(WestAndEast, NorthAndEast, NorthAndSouth, SouthAndEast, SouthAndWest).map(Pipe.apply),
        Vector(
          Pipe(SouthAndWest),
          Start,
          Pipe(WestAndEast),
          Pipe(SouthAndWest),
          Pipe(NorthAndSouth)
        ),
        Vector(NorthAndEast, NorthAndSouth, SouthAndWest, NorthAndSouth, NorthAndSouth).map(Pipe.apply),
        Vector(WestAndEast, NorthAndEast, WestAndEast, NorthAndWest, NorthAndSouth).map(Pipe.apply),
        Vector(NorthAndEast, NorthAndSouth, WestAndEast, NorthAndWest, SouthAndEast).map(Pipe.apply)
      )
    )
    assertEquals(Field.parse(smallInput1), Some(expected))

  test("parsing smallSimplifiedInput1"):
    val expected = Field(rows =
      Vector(
        Vector.fill(5)(Ground),
        Vector(Ground, Start, Pipe(WestAndEast), Pipe(SouthAndWest), Ground),
        Vector(Ground, Pipe(NorthAndSouth), Ground, Pipe(NorthAndSouth), Ground),
        Vector(Ground, Pipe(NorthAndEast), Pipe(WestAndEast), Pipe(NorthAndWest), Ground),
        Vector.fill(5)(Ground)
      )
    )
    assertEquals(Field.parse(smallSimplifiedInput1), Some(expected))

  test("start position for smallInput1 is Pos(row = 1, col = 1)"):
    assertEquals(Field.parse(smallInput1).flatMap(_.startPos), Some(Pos(row = 1, col = 1)))

  test("start position for smallSimplifiedInput1 is Pos(row = 1, col = 1)"):
    assertEquals(
      Field.parse(smallSimplifiedInput1).flatMap(_.startPos),
      Some(Pos(row = 1, col = 1))
    )

  test("start position for smallInput2 is Pos(row = 2, col = 0)"):
    assertEquals(Field.parse(smallInput2).flatMap(_.startPos), Some(Pos(row = 2, col = 0)))

  test("start position for smallSimplifiedInput2 is Pos(row = 2, col = 0)"):
    assertEquals(
      Field.parse(smallSimplifiedInput2).flatMap(_.startPos),
      Some(Pos(row = 2, col = 0))
    )

  test("start position for big input is defined"):
    assert(Field.parse(bigInput).flatMap(_.startPos).isDefined)

  test("one-step moves from start found for smallInput1"):
    assertEquals(
      for {
        field <- Field.parse(smallInput1)
        start <- field.startPos
      } yield field.oneStepFrom(start),
      Some(Set(Pos(row = 1, col = 2), Pos(row = 2, col = 1)))
    )

  test("one-step moves from start found for smallSimplifiedInput1"):
    assertEquals(
      for {
        field <- Field.parse(smallSimplifiedInput1)
        start <- field.startPos
      } yield field.oneStepFrom(start),
      Some(Set(Pos(row = 1, col = 2), Pos(row = 2, col = 1)))
    )

  test("one-step moves from start found for smallInput2"):
    assertEquals(
      for {
        field <- Field.parse(smallInput2)
        start <- field.startPos
      } yield field.oneStepFrom(start),
      Some(Set(Pos(row = 2, col = 1), Pos(row = 3, col = 0)))
    )

  test("one-step moves from start found for smallSimplifiedInput2"):
    assertEquals(
      for {
        field <- Field.parse(smallSimplifiedInput2)
        start <- field.startPos
      } yield field.oneStepFrom(start),
      Some(Set(Pos(row = 2, col = 1), Pos(row = 3, col = 0)))
    )

  test("#2 one-step moves from start found for big input"):
    assertEquals(
      for {
        field <- Field.parse(bigInput)
        start <- field.startPos
      } yield field.oneStepFrom(start).size,
      Some(2)
    )

  test("loop for smallInput1"):
    assertEquals(
      Field.parse(smallInput1).flatMap(_.loop),
      Some(
        Loop(
          firstPath = NonEmptyList.of(Pos(1, 1), Pos(2, 1), Pos(3, 1), Pos(3, 2), Pos(3, 3)),
          secondPath = NonEmptyList.of(Pos(1, 1), Pos(1, 2), Pos(1, 3), Pos(2, 3), Pos(3, 3))
        )
      )
    )

  test("loop for smallInput1 is same of smallSimplifiedInput1"):
    assertEquals(
      Field.parse(smallInput1).flatMap(_.loop),
      Field.parse(smallSimplifiedInput1).flatMap(_.loop)
    )

  test("loop for smallInput2"):
    assertEquals(
      Field.parse(smallInput2).flatMap(_.loop),
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

  test("loop for smallInput2 is same of smallSimplifiedInput2"):
    assertEquals(
      Field.parse(smallInput2).flatMap(_.loop),
      Field.parse(smallSimplifiedInput2).flatMap(_.loop)
    )

  test("steps count to farthest point in loop is 4 for smallInput1"):
    assertEquals(stepsCountToFarthestInLoop(smallInput1), Some(4))

  test("steps count to farthest point in loop is same for smallInput1 and simplified one"):
    assertEquals(
      stepsCountToFarthestInLoop(smallInput1),
      stepsCountToFarthestInLoop(smallSimplifiedInput1)
    )

  test("steps count to farthest point in loop is 8 for smallInput2"):
    assertEquals(stepsCountToFarthestInLoop(smallInput2), Some(8))

  test("steps count to farthest point in loop is same for smallInput2 and simplified one"):
    assertEquals(
      stepsCountToFarthestInLoop(smallInput2),
      stepsCountToFarthestInLoop(smallSimplifiedInput2)
    )

  test("loop branches has same length for big input"):
    assertEquals(
      Field.parse(bigInput).flatMap(_.loop.map(l => l.firstPath.length == l.secondPath.length)),
      Some(true)
    )

  test("steps count to farthest point in loop is 6_828 for big input"):
    assertEquals(stepsCountToFarthestInLoop(bigInput), Some(6_828))

  property("cardinal direction of p.north is Some(North), for any p: Pos"):
    forAll(posGen)(p => assertEquals(p.cardinalDirectionOf(p.north), Some(North)))

  property("cardinal direction of p.south is Some(South), for any p: Pos"):
    forAll(posGen)(p => assertEquals(p.cardinalDirectionOf(p.south), Some(South)))

  property("cardinal direction of p.west is Some(West), for any p: Pos"):
    forAll(posGen)(p => assertEquals(p.cardinalDirectionOf(p.west), Some(West)))

  property("cardinal direction of p.east is Some(East), for any p: Pos"):
    forAll(posGen)(p => assertEquals(p.cardinalDirectionOf(p.east), Some(East)))

  property("cardinal direction of p.north.east is None, for any p: Pos"):
    forAll(posGen)(p => assertEquals(p.cardinalDirectionOf(p.north.east), None))

  property("inversions to East are empty when loop is NOT in the row, from any column and for any row"):
    import TileRawType.*
    val n = 10
    forAll(
      Gen.zip(
        Gen.choose(0, n - 1),
        pipeTypeGen,
        tilesRowGen(n).map(_.map((_, NotOnLoop: TileRawType)))
      )
    ) { (fromCol, startAs, row) =>
      assertEquals(Field.inversionsToEast(fromCol, startAs, row), List.empty)
    }

  test(
    "inversions to East are [Vertical, NorthToSouth, Vertical, SouthToNorth]" +
      " for row with loop \"..|..LS7|.F--J.\", 'S' as '-', from leftmost column"
  ):
    import TileRawType.*
    assertEquals(
      Field.inversionsToEast(
        fromCol = 0,
        startAs = WestAndEast,
        row = Vector(
          (Ground, NotOnLoop),
          (Ground, NotOnLoop),
          (Pipe(NorthAndSouth), OnLoop),
          (Ground, NotOnLoop),
          (Ground, NotOnLoop),
          (Pipe(NorthAndEast), OnLoop),
          (Start, OnLoop),
          (Pipe(SouthAndWest), OnLoop),
          (Pipe(NorthAndSouth), OnLoop),
          (Ground, NotOnLoop),
          (Pipe(SouthAndEast), OnLoop),
          (Pipe(WestAndEast), OnLoop),
          (Pipe(WestAndEast), OnLoop),
          (Pipe(NorthAndWest), OnLoop),
          (Ground, NotOnLoop)
        )
      ),
      List(Vertical, NorthToSouth, Vertical, SouthToNorth)
    )

  test("Field from smallInput3 has 4 tiles inside the loop"):
    assertEquals(countTilesInsideLoop(smallInput3), Some(4))

  test("Field from smallInput4 has 8 tiles inside the loop"):
    assertEquals(countTilesInsideLoop(smallInput4), Some(8))

  test("Field from smallInput5 has 10 tiles inside the loop"):
    assertEquals(countTilesInsideLoop(smallInput5), Some(10))

  test("Field from bigInput has 459 tiles inside the loop"):
    assertEquals(countTilesInsideLoop(bigInput), Some(459))

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

  val smallInput3: List[String] = List(
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  )

  val smallInput4: List[String] = List(
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  )

  val smallInput5: List[String] = List(
    "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L"
  )

  def posGen: Gen[Pos] = Gen.zip(Gen.posNum[Int], Gen.posNum[Int]).map(Pos.apply)

  def tilesRowGen(n: Int): Gen[Vector[Tile]] = Gen.listOfN(n, tileGen).map(_.toVector)
  def tileGen: Gen[Tile] = Gen.oneOf(Gen.const(Ground), Gen.const(Start), pipeTypeGen.map(Pipe.apply))
  def pipeTypeGen: Gen[PipeType] = Gen.oneOf(PipeType.values.toList)
