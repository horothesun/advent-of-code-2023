import Day13.*
import Day13.Terrain.*
import Day13Suite.*
import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day13Suite extends ScalaCheckSuite:

  property("hCut(topHeight = 0) always returns None on any NonEmptyMatrix"):
    forAll(nonEmptyMatrixGen(Gen.alphaUpperChar)) { nem =>
      assertEquals(nem.hCut(topHeight = 0), None)
    }

  test("hCut returns correct defined result on NonEmptyMatrix with height = 3 and topHeight = 2"):
    val r1 = NonEmptyList.of(1, 2)
    val r2 = NonEmptyList.of(3, 4)
    val r3 = NonEmptyList.of(5, 6)
    assertEquals(
      NonEmptyMatrix(r1, r2, r3).hCut(topHeight = 2),
      Some((NonEmptyMatrix(r1, r2), NonEmptyMatrix(r3)))
    )

  property("hCut returns None on NonEmptyMatrix with height > 1 and topHeight >= height"):
    forAll(for {
      h <- Gen.choose(2, 10)
      w <- Gen.choose(1, 10)
      nem <- nonEmptyMatrixGen(Gen.alphaUpperChar, w, h)
      th <- Gen.choose(h, 20)
    } yield (nem, th))((nem, topHeight) => assertEquals(nem.hCut(topHeight), None))

  property("hCut preserves height on NonEmptyMatrix with height > 1 and 1 <= topHeight < height"):
    forAll(for {
      h <- Gen.choose(2, 10)
      w <- Gen.choose(1, 10)
      nem <- nonEmptyMatrixGen(Gen.alphaUpperChar, w, h)
      th <- Gen.choose(1, h - 1)
    } yield (nem, th)) { (nem, topHeight) =>
      nem.hCut(topHeight) match
        case Some((t, b)) => assertEquals(nem.height, t.height + b.height)
        case None         => fail("hCut failure")
    }

  test("Pattern parse on small input 1"):
    assertEquals(
      Pattern.parse(smallInput1),
      Some(
        Pattern(
          NonEmptyMatrix(
            NonEmptyList.of(Rocks, Ash, Rocks, Rocks, Ash, Ash, Rocks, Rocks, Ash),
            NonEmptyList.of(Ash, Ash, Rocks, Ash, Rocks, Rocks, Ash, Rocks, Ash),
            NonEmptyList.of(Rocks, Rocks, Ash, Ash, Ash, Ash, Ash, Ash, Rocks),
            NonEmptyList.of(Rocks, Rocks, Ash, Ash, Ash, Ash, Ash, Ash, Rocks),
            NonEmptyList.of(Ash, Ash, Rocks, Ash, Rocks, Rocks, Ash, Rocks, Ash),
            NonEmptyList.of(Ash, Ash, Rocks, Rocks, Ash, Ash, Rocks, Rocks, Ash),
            NonEmptyList.of(Rocks, Ash, Rocks, Ash, Rocks, Rocks, Ash, Rocks, Ash)
          )
        )
      )
    )

  test("List splitBy with internal separators"):
    assertEquals(
      List('a', 'b', 'c', '|', 'd', 'e', '|', 'f').splitBy(separator = '|'),
      List(List('a', 'b', 'c'), List('d', 'e'), List('f'))
    )

  test("List splitBy with leading separator"):
    assertEquals(
      List('|', 'a', 'b', 'c').splitBy(separator = '|'),
      List(List.empty, List('a', 'b', 'c'))
    )

  test("List splitBy with trailing separator"):
    assertEquals(
      List('a', 'b', 'c', '|').splitBy(separator = '|'),
      List(List('a', 'b', 'c'), List.empty)
    )

  property("List splitBy with no separator returns the 'singleton-ed' list"):
    forAll(Gen.listOf(Gen.alphaChar))(cs => assertEquals(cs.splitBy(separator = '|'), List(cs)))

  test("splitBy(\"\") small input returns its 2 parts"):
    assertEquals(smallInput.splitBy(separator = ""), List(smallInput1, smallInput2))

  test("reflectionLeftWidths on small input 1 is List(5)"):
    assertEquals(Pattern.parse(smallInput1).map(_.reflectionLeftWidths), Some(List(5)))

  test("reflectionTopHeights on small input 2 is List(4)"):
    assertEquals(Pattern.parse(smallInput2).map(_.reflectionTopHeights), Some(List(4)))

  test("notes summary for small input is 405"):
    assertEquals(notesSummary(smallInput), Some(405L))

  test("notes summary for big input is 35_521"):
    assertEquals(notesSummary(bigInput), Some(35_521L))

object Day13Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day13_input.txt")

  val smallInput1: List[String] = List(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#."
  )

  val smallInput2: List[String] = List(
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  )

  val smallInput: List[String] = smallInput1 ++ ("" :: smallInput2)

  def nonEmptyMatrixGen[A](aGen: Gen[A]): Gen[NonEmptyMatrix[A]] =
    Gen.zip(Gen.choose(1, 10), Gen.choose(1, 10)).flatMap((w, h) => nonEmptyMatrixGen(aGen, w, h))
  def nonEmptyMatrixGen[A](aGen: Gen[A], width: Int, height: Int): Gen[NonEmptyMatrix[A]] =
    nonEmptyListGen(nonEmptyListGen(aGen, width), height).map(NonEmptyMatrix.apply)

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)
