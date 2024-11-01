import Day12.*
import Day12Suite.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day12Suite extends ScalaCheckSuite:

  test("Row[RawCondition] parsed from \"???.###\""):
    import RawCondition.*
    assertEquals(
      parseRawConditionRow("???.###"),
      Some(Row.of[RawCondition](Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged))
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

  property("interleaving / filtering round-trip"):
    forAll(Gen.listOf(Gen.posNum[Int]), Gen.listOf(Gen.negNum[Int])) { (pos, neg) =>
      val interleaved = pos.interleavedLeft(neg)
      assertEquals(interleaved.filter(_ > 0), pos)
      assertEquals(interleaved.filter(_ < 0), neg)
    }

object Day12Suite:

//  val bigInput: List[String] = getLinesFromFile("src/test/scala/day12_input.txt")

  def rowGen[A](aGen: Gen[A]): Gen[Row[A]] = Gen.posNum[Int].flatMap(rowGen(aGen, _))
  def rowGen[A](aGen: Gen[A], length: Int): Gen[Row[A]] = nonEmptyListGen(aGen, length).map(Row.apply)

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)
