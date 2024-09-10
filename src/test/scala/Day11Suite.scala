import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day11.*
import Day11.Pixel.*
import Day11Suite.*

class Day11Suite extends ScalaCheckSuite:

  test("non-empty matrix clockwise rotation"):
    assertEquals(
      NonEmptyMatrix(
        rows = NonEmptyList.of(
          NonEmptyList.of(1, 2),
          NonEmptyList.of(3, 4),
          NonEmptyList.of(5, 6)
        )
      ).rotatedCW,
      NonEmptyMatrix(
        rows = NonEmptyList.of(
          NonEmptyList.of(5, 3, 1),
          NonEmptyList.of(6, 4, 2)
        )
      )
    )

  property("rotating CW and then CCW does NOT change any non-empty matrix"):
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(m => assertEquals(m.rotatedCW.rotatedCCW, m))

  property("rotating CCW and then CW does NOT change any non-empty matrix"):
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(m => assertEquals(m.rotatedCCW.rotatedCW, m))

  property("rotating CW four times does NOT change any non-empty matrix"):
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(m => assertEquals(m.rotatedCW.rotatedCW.rotatedCW.rotatedCW, m))

  property("rotating CCW four times does NOT change any non-empty matrix"):
    forAll(nonEmptyMatrixGen(Gen.alphaChar))(m => assertEquals(m.rotatedCCW.rotatedCCW.rotatedCCW.rotatedCCW, m))

  test("non-empty matrix zip-with-position"):
    assertEquals(
      NonEmptyMatrix(
        rows = NonEmptyList.of(
          NonEmptyList.of('a', 'b', 'c'),
          NonEmptyList.of('d', 'e', 'f')
        )
      ).zipWithPos,
      NonEmptyMatrix(
        rows = NonEmptyList.of(
          NonEmptyList.of(('a', Pos(0, 0)), ('b', Pos(0, 1)), ('c', Pos(0, 2))),
          NonEmptyList.of(('d', Pos(1, 0)), ('e', Pos(1, 1)), ('f', Pos(1, 2)))
        )
      )
    )

  test("parsing really small image successfully"):
    assertEquals(
      Image.parse(
        input = List(
          ".#.",
          "...",
          "..#"
        )
      ),
      Some(
        Image(
          rows = NonEmptyList.of(
            NonEmptyList.of(EmptySpace, Galaxy, EmptySpace),
            NonEmptyList.of(EmptySpace, EmptySpace, EmptySpace),
            NonEmptyList.of(EmptySpace, EmptySpace, Galaxy)
          )
        )
      )
    )

  test("expanded small input 1"):
    assertEquals(Image.parse(smallInput1).map(_.expanded), Image.parse(expandedSmallInput1))

  test("all galaxies positions for really small image"):
    val image = Image(
      rows = NonEmptyList.of(
        NonEmptyList.of(EmptySpace, Galaxy, EmptySpace),
        NonEmptyList.of(EmptySpace, EmptySpace, EmptySpace),
        NonEmptyList.of(EmptySpace, EmptySpace, Galaxy)
      )
    )
    assertEquals(image.allGalaxies, Set(Pos(0, 1), Pos(2, 2)))

  test("all unique pairs"):
    assertEquals(
      allUniquePairs(Set(1, 2, 3, 4)),
      Set((1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4))
    )

  test("Manhattan distance between Pos(0,1) and Pos(2,2) is 3"):
    assertEquals(Pos(0, 1).manhattanDistanceTo(Pos(2, 2)), 3)

  property("Manhattan distances p1->p2 and p2->p1 are the same for any p1, p2: Pos"):
    forAll(posGen, posGen) { (p1, p2) =>
      assertEquals(p1.manhattanDistanceTo(p2), p2.manhattanDistanceTo(p1))
    }

object Day11Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day11_input.txt")

  val smallInput1: List[String] = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )
  val expandedSmallInput1: List[String] = List(
    "....#........",
    ".........#...",
    "#............",
    ".............",
    ".............",
    "........#....",
    ".#...........",
    "............#",
    ".............",
    ".............",
    ".........#...",
    "#....#......."
  )

  def nonEmptyMatrixGen[A](aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    Gen.zip(Gen.choose(1, 10), Gen.choose(1, 10)).flatMap((w, h) => nonEmptyMatrixGen(aGen, w, h))
  def nonEmptyMatrixGen[A](aGen: Gen[A], width: Int, height: Int): Gen[NonEmptyMatrix[A]] =
    nonEmptyListGen(nonEmptyListGen(aGen, width), height).map(NonEmptyMatrix.apply)

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)

  def posGen: Gen[Pos] = Gen.zip(Gen.posNum[Int], Gen.posNum[Int]).map(Pos.apply)
