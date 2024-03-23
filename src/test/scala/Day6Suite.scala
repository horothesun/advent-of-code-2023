import munit.ScalaCheckSuite
import Day6Suite.*

class Day6Suite extends ScalaCheckSuite:

  test("parseAt[Int](identity, \"      7  15   30\")") {
    assertEquals(parseAs[Int](identity, "      7  15   30"), Some(List(7, 15, 30)))
  }

  test("parseTimes(\"Time:      7  15   30\")") {
    assertEquals(parseTimes("Time:      7  15   30"), Some(List(Time(7), Time(15), Time(30))))
  }

  test("parseDistances(\"Distance:  9  40  200\")") {
    assertEquals(
      parseDistances("Distance:  9  40  200"),
      Some(List(Distance(9), Distance(40), Distance(200)))
    )
  }

  test("smallInput parses correctly") {
    assertEquals(
      parseRaces(smallInput),
      Some(
        List(
          Race(allowance = Time(7), best = Distance(9)),
          Race(allowance = Time(15), best = Distance(40)),
          Race(allowance = Time(30), best = Distance(200))
        )
      )
    )
  }

  test("bigInput parses correctly") {
    assertEquals(
      parseRaces(bigInput),
      Some(
        List(
          Race(allowance = Time(61), best = Distance(643)),
          Race(allowance = Time(70), best = Distance(1184)),
          Race(allowance = Time(90), best = Distance(1362)),
          Race(allowance = Time(66), best = Distance(1041))
        )
      )
    )
  }

object Day6Suite:

  val bigInput: List[String] = List(
    "Time:        61     70     90     66",
    "Distance:   643   1184   1362   1041"
  )

  val smallInput: List[String] = List(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )
