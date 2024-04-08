import cats.data.NonEmptyList
import cats.implicits.*
import munit.ScalaCheckSuite
import Day9.*
import Day9Suite.*

class Day9Suite extends ScalaCheckSuite:

  test("\"0 3 6 9 12 15\" differences are \"3 3 3 3 3\"") {
    assertEquals(
      differences(NonEmptyList.of(0, 3, 6, 9, 12, 15)),
      Some(NonEmptyList.of[Long](3, 3, 3, 3, 3))
    )
  }

  test("\"1 3 6 10 15 21\" differences are \"2 3 4 5 6\"") {
    assertEquals(
      differences(NonEmptyList.of(1, 3, 6, 10, 15, 21)),
      Some(NonEmptyList.of[Long](2, 3, 4, 5, 6))
    )
  }

  test("\"10 13 16 21 30 45\" differences are \"3 3 5 9 15\"") {
    assertEquals(
      differences(NonEmptyList.of(10, 13, 16, 21, 30, 45)),
      Some(NonEmptyList.of[Long](3, 3, 5, 9, 15))
    )
  }

  test("\"0 3 6 9 12 15\" all differences are \"3 3 3 3 3\", \"0 0 0 0\"") {
    assertEquals(
      allDifferences(History(NonEmptyList.of(0, 3, 6, 9, 12, 15))),
      List(
        NonEmptyList.of[Long](3, 3, 3, 3, 3),
        NonEmptyList.of[Long](0, 0, 0, 0)
      )
    )
  }

  test("\"1 3 6 10 15 21\" all differences are \"2 3 4 5 6\", \"1 1 1 1\", \"0 0 0\"") {
    assertEquals(
      allDifferences(History(NonEmptyList.of(1, 3, 6, 10, 15, 21))),
      List(
        NonEmptyList.of[Long](2, 3, 4, 5, 6),
        NonEmptyList.of[Long](1, 1, 1, 1),
        NonEmptyList.of[Long](0, 0, 0)
      )
    )
  }

  test("\"10 13 16 21 30 45\" all differences are \"3 3 5 9 15\", \"0 2 4 6\", \"2 2 2\", \"0 0\"") {
    assertEquals(
      allDifferences(History(NonEmptyList.of(10, 13, 16, 21, 30, 45))),
      List(
        NonEmptyList.of[Long](3, 3, 5, 9, 15),
        NonEmptyList.of[Long](0, 2, 4, 6),
        NonEmptyList.of[Long](2, 2, 2),
        NonEmptyList.of[Long](0, 0)
      )
    )
  }

  test("\"0 3 6 9 12 15\" all predictions are List(18, 3, 0)") {
    assertEquals(allPredictions(History(NonEmptyList.of(0, 3, 6, 9, 12, 15))), List(18L, 3L, 0L))
  }

  test("\"1 3 6 10 15 21\" all predictions are List(28, 7, 1, 0)") {
    assertEquals(allPredictions(History(NonEmptyList.of(1, 3, 6, 10, 15, 21))), List(28L, 7L, 1L, 0L))
  }

  test("\"10 13 16 21 30 45\" all predictions are List(68, 23, 8, 2, 0)") {
    assertEquals(
      allPredictions(History(NonEmptyList.of(10, 13, 16, 21, 30, 45))),
      List(68L, 23L, 8L, 2L, 0L)
    )
  }

  test("small input all prediction sum is 114") {
    assertEquals(allPredictionsSum(smallInput), Some(114L))
  }

  test("big input all prediction sum is 1_702_218_515") {
    assertEquals(allPredictionsSum(bigInput), Some(1_702_218_515L))
  }

  test("\"10 13 16 21 30 45\" all past predictions are List(5, 5, -2, 2, 0)") {
    assertEquals(
      allPastPredictions(History(NonEmptyList.of(10, 13, 16, 21, 30, 45))),
      List(5L, 5L, -2L, 2L, 0L)
    )
  }

  test("small input all past prediction sum is 2") {
    assertEquals(allPastPredictionsSum(smallInput), Some(2L))
  }

  test("big input all past prediction sum is 925") {
    assertEquals(allPastPredictionsSum(bigInput), Some(925L))
  }

object Day9Suite:

  val smallInput: NonEmptyList[String] = NonEmptyList.of(
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  )

  val bigInput: NonEmptyList[String] = getLinesFromFile("src/test/scala/day9_input.txt").toNel.get
