import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day1Suite.*

class Day1Suite extends ScalaCheckSuite {

  test("day1 == 42") {
    assertEquals(day1, 42)
  }

}
object Day1Suite {

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day1_input.txt")

}
