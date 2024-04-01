import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day6.*
import Day6Suite.*

class Day6Suite extends ScalaCheckSuite:

  test("get all possible distances from time allowance") {
    assertEquals(
      getAllPossibleDistances(allowance = Time(7)),
      List[Long](0, 6, 10, 12, 12, 10, 6, 0).map(Distance.apply)
    )
  }

  test("(brute force) number of ways to beat the 9mm record with a 7ms allowance is 4") {
    assertEquals(countWaysToWin_bruteForce(Race(allowance = Time(7), record = Distance(9))), 4L)
  }

  test("(brute force) number of ways to beat the 40mm record with a 15ms allowance is 8") {
    assertEquals(countWaysToWin_bruteForce(Race(allowance = Time(15), record = Distance(40))), 8L)
  }

  test("(brute force) number of ways to beat the 200mm record with a 30ms allowance is 9") {
    assertEquals(countWaysToWin_bruteForce(Race(allowance = Time(30), record = Distance(200))), 9L)
  }

  test("(brute force) multiplied ways to win on small input is 288") {
    val races = List(
      Race(allowance = Time(7), record = Distance(9)),
      Race(allowance = Time(15), record = Distance(40)),
      Race(allowance = Time(30), record = Distance(200))
    )
    assertEquals(getMultipliedWaysToWin_bruteForce(races), 288L)
  }

  test("(brute force) multiplied ways to win on big input is 293_046") {
    val races = List(
      Race(allowance = Time(61), record = Distance(643)),
      Race(allowance = Time(70), record = Distance(1184)),
      Race(allowance = Time(90), record = Distance(1362)),
      Race(allowance = Time(66), record = Distance(1041))
    )
    assertEquals(getMultipliedWaysToWin_bruteForce(races), 293_046L)
  }

  property("countWaysToWin(r) == countWaysToWin_bruteForce(r)") {
    forAll(raceGen)(r => assertEquals(countWaysToWin(r), countWaysToWin_bruteForce(r)))
  }

  test("number of ways to beat the 9mm record with a 7ms allowance is 4") {
    assertEquals(countWaysToWin(Race(allowance = Time(7), record = Distance(9))), 4L)
  }

  test("number of ways to beat the 40mm record with a 15ms allowance is 8") {
    assertEquals(countWaysToWin(Race(allowance = Time(15), record = Distance(40))), 8L)
  }

  test("number of ways to beat the 200mm record with a 30ms allowance is 9") {
    assertEquals(countWaysToWin(Race(allowance = Time(30), record = Distance(200))), 9L)
  }

  test("number of ways to beat the small single race is 71_503") {
    val smallSingleRace = Race(allowance = Time(71_530), record = Distance(940_200))
    assertEquals(countWaysToWin(smallSingleRace), 71_503L)
  }

  test("number of ways to beat the BIG single race is 35_150_181") {
    val bigSingleRace = Race(allowance = Time(61_709_066), record = Distance(643_118_413_621_041L))
    assertEquals(countWaysToWin(bigSingleRace), 35_150_181L)
  }

object Day6Suite:

  def raceGen: Gen[Race] = for {
    t <- Gen.choose(min = 0L, max = 500_000L)
    d <- Gen.choose(min = t, max = t + 10_000L)
  } yield Race(allowance = Time(t), record = Distance(d))
