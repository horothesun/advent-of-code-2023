import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day11.*
import Day11Suite.*

class Day11Suite extends ScalaCheckSuite:

  test("day11 == 42"):
    assertEquals(day11, 42)

object Day11Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day11_input.txt")
