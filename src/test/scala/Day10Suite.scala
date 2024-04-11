import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day10.*
import Day10Suite.*

class Day10Suite extends ScalaCheckSuite:

  test("day10 == 42") {
    assertEquals(day10, 42)
  }

object Day10Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day10_input.txt")
