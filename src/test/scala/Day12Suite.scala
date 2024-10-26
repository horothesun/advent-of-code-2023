import Day12.*
import Day12.Condition.*
import Day12Suite.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day12Suite extends ScalaCheckSuite:

  test("Row[Condition] parsed from \"???.###\""):
    assertEquals(
      parseConditionRow("???.###"),
      Some(Row.of[Condition](Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged))
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

object Day12Suite:

//  val bigInput: List[String] = getLinesFromFile("src/test/scala/day12_input.txt")

  def rowGen[A](aGen: Gen[A]): Gen[Row[A]] = Gen.posNum[Int].flatMap(rowGen(aGen, _))
  def rowGen[A](aGen: Gen[A], length: Int): Gen[Row[A]] = nonEmptyListGen(aGen, length).map(Row.apply)

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)
