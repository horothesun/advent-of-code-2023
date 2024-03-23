import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day6Suite.*

class Day6Suite extends ScalaCheckSuite:

  test("day6 == 42") {
    assertEquals(day6, 42)
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
