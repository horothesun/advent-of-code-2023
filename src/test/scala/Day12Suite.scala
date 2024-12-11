import Day12.*
import Day12Suite.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.Stream
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day12Suite extends ScalaCheckSuite:

  test("Row[RawCondition] parsed from \"???.###\""):
    import RawCondition.*
    assertEquals(
      ConditionRecord.parseRawConditionRow("???.###"),
      Some(Row.of[RawCondition](Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged))
    )

  test("ConditionRecord parsed from \"???.### 1,1,3\""):
    import RawCondition.*
    assertEquals(
      ConditionRecord.parse("???.### 1,1,3"),
      Some(
        ConditionRecord(
          rawConditions = Row.of[RawCondition](Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged),
          contiguousDamagedGroupSizes = List(1, 1, 3)
        )
      )
    )

  test("Row to contiguous groups list example"):
    assertEquals(
      Row.of('a', 'b', 'b', 'c', 'd', 'd', 'd', 'e').toContiguous,
      NonEmptyList.of(
        Contiguous('a', reps = 1),
        Contiguous('b', reps = 2),
        Contiguous('c', reps = 1),
        Contiguous('d', reps = 3),
        Contiguous('e', reps = 1)
      )
    )

  test("Row to contiguous groups list preserves size"):
    forAll(rowGen(Gen.alphaNumChar)) { r =>
      assertEquals(r.values.length, r.toContiguous.map(_.reps).sumAll)
    }

  test("Condition.valuesNes and Condition.values have same elements"):
    assertEquals(Condition.valuesNes.toList.sorted, Condition.values.toList.sorted)

  test("interleaving List(1,2,3,4) and List(10,20) is List(1,10,2,20,3,4)"):
    assertEquals(
      List(1, 2, 3, 4).interleavedLeft(List(10, 20)),
      List(1, 10, 2, 20, 3, 4)
    )

  test("interleaving List(1,2) and List(10,20,30,40) is List(1,10,2,20,30,40)"):
    assertEquals(
      List(1, 2).interleavedLeft(List(10, 20, 30, 40)),
      List(1, 10, 2, 20, 30, 40)
    )

  property("interleaving any two lists preserves total size"):
    val alphaNumCharsGen = Gen.listOf(Gen.alphaNumChar)
    forAll(alphaNumCharsGen, alphaNumCharsGen) { (fst, snd) =>
      assertEquals(fst.interleavedLeft(snd).length, fst.length + snd.length)
    }

  property("interleaving any list 'l' with an empty list is 'l'"):
    forAll(Gen.listOf(Gen.alphaNumChar)) { l =>
      assertEquals(l.interleavedLeft(List.empty), l)
    }

  property("interleaving an empty list with any list 'l' is 'l'"):
    forAll(Gen.listOf(Gen.alphaNumChar)) { l =>
      assertEquals(List.empty[Char].interleavedLeft(l), l)
    }

  property("interleaving -> filtering round-trip"):
    forAll(Gen.listOf(Gen.posNum[Int]), Gen.listOf(Gen.negNum[Int])) { (pos, neg) =>
      val interleaved = pos.interleavedLeft(neg)
      assertEquals(interleaved.filter(_ > 0), pos)
      assertEquals(interleaved.filter(_ < 0), neg)
    }

  test("all possible Condition combinations of length 20 are 1_048_576"):
    assertEquals(allCombinationsOf(length = 20, values = Condition.valuesNes).length, 1_048_576)

  property("all possible Condition combinations (eagerly) non-tailrec and tailrec calculated are the same"):
    forAll(Gen.choose(min = 0, max = 10)) { length =>
      assertEquals(
        allCombinationsOf_rec(length, values = Condition.valuesNes),
        allCombinationsOf(length, values = Condition.valuesNes)
      )
    }

  property("all possible Condition combinations eagerly (non-tailrec) and lazily calculated are the same"):
    forAll(Gen.choose(min = 0, max = 10)) { length =>
      assertEquals(
        allCombinationsOf_rec(length, values = Condition.valuesNes),
        allCombinationsOf_lzy(length, values = Condition.valuesNes).toList
      )
    }

  test("damaged group sizes match between #.#.### and 1,1,3"):
    import Condition.*
    assertEquals(
      damagedGroupSizesCheck(
        Row.of[Condition](Damaged, Operational, Damaged, Operational, Damaged, Damaged, Damaged),
        List(1, 1, 3)
      ),
      ConditionsCheck.Match
    )

  test("damaged group sizes match between .#.###.#.###### and 1,3,1,6"):
    import Condition.*
    assertEquals(
      damagedGroupSizesCheck(
        Row.of[Condition](
          Operational,
          Damaged,
          Operational,
          Damaged,
          Damaged,
          Damaged,
          Operational,
          Damaged,
          Operational,
          Damaged,
          Damaged,
          Damaged,
          Damaged,
          Damaged,
          Damaged
        ),
        List(1, 3, 1, 6)
      ),
      ConditionsCheck.Match
    )

  test("damaged group sizes do NOT match between #.#.#. and 1,1,3"):
    import Condition.*
    assertEquals(
      damagedGroupSizesCheck(
        Row.of[Condition](Damaged, Operational, Damaged, Operational, Damaged, Operational),
        List(1, 1, 3)
      ),
      ConditionsCheck.Fail
    )

  property("all arrangements for any fully determined RawCondition are only the original"):
    forAll(rowGen(conditionGen)) { conditionRow =>
      val fullyDeterminedRawConditionRow = conditionRow.map(_.toRaw)
      assertEquals(
        getAllArrangements(fullyDeterminedRawConditionRow),
        NonEmptyList.one(conditionRow)
      )
    }

  test("all arrangements for #?.??.#"):
    assertEquals(
      getAllArrangements {
        import RawCondition.*
        Row.of[RawCondition](Damaged, Unknown, Operational, Unknown, Unknown, Operational, Damaged)
      }, {
        import Condition.*
        NonEmptyList.of(
          Row.of[Condition](Damaged, Operational, Operational, Operational, Operational, Operational, Damaged),
          Row.of[Condition](Damaged, Operational, Operational, Operational, Damaged, Operational, Damaged),
          Row.of[Condition](Damaged, Operational, Operational, Damaged, Operational, Operational, Damaged),
          Row.of[Condition](Damaged, Operational, Operational, Damaged, Damaged, Operational, Damaged),
          Row.of[Condition](Damaged, Damaged, Operational, Operational, Operational, Operational, Damaged),
          Row.of[Condition](Damaged, Damaged, Operational, Operational, Damaged, Operational, Damaged),
          Row.of[Condition](Damaged, Damaged, Operational, Damaged, Operational, Operational, Damaged),
          Row.of[Condition](Damaged, Damaged, Operational, Damaged, Damaged, Operational, Damaged)
        )
      }
    )

  test("total valid arrangements for small input are 21"):
    assertEquals(totalValidArrangements(smallInput), Some(21))

  test("total valid arrangements (lazily evaluated) for small input are 21"):
    assertEquals(totalValidArrangements_lzy(smallInput), Some(21))

//  test("total valid arrangements for big input are 7_169"):
//    assertEquals(totalValidArrangements(bigInput), Some(7_169))

//  override val munitTimeout: scala.concurrent.duration.Duration =
//    import scala.concurrent.duration.{Duration, MINUTES}
//    Duration(2, MINUTES)
//
//  test("total valid arrangements (lazily evaluated) for big input are 7_169"):
//    assertEquals(totalValidArrangements_lzy(bigInput), Some(7_169))

//  test("..."):
//    val combinationSize = 5 * 20
//    val firstN = 1_000_000
//    assertEquals(
//      allCombinationsOf_lzy(combinationSize, values = Condition.valuesNes)
//        .take(firstN)
//        .map(_.length)
//        .fold(0L)(_ + _)
//        .toList,
//      // List.fill[Int](firstN)(combinationSize)
//      Stream.emit(combinationSize).repeatN(firstN).fold(0L)(_ + _).toList
//    )

object Day12Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day12_input.txt")

  val smallInput: List[String] = List(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  )

  def conditionGen: Gen[Condition] = Gen.oneOf(Condition.values.toList)

  def rowGen[A](aGen: Gen[A]): Gen[Row[A]] = Gen.posNum[Int].flatMap(rowGen(aGen, _))
  def rowGen[A](aGen: Gen[A], length: Int): Gen[Row[A]] = nonEmptyListGen(aGen, length).map(Row.apply)

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)
