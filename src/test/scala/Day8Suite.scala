import munit.ScalaCheckSuite
import Day8.*
import Day8Suite.*

class Day8Suite extends ScalaCheckSuite:

  test("parse \"AAA = (BBB, CCC)\" node") {
    assertEquals(
      Node.parse("AAA = (BBB, CCC)"),
      Some(Node(id = NodeId("AAA"), left = NodeId("BBB"), right = NodeId("CCC")))
    )
  }

object Day8Suite:

//  val bigInput: List[String] = getLinesFromFile("src/test/scala/day8_input.txt")

  val smallInput1: List[String] = List(
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val smallInput2: List[String] = List(
    "LLR",
    "",
    "AAA, = (BBB, BBB)",
    "BBB, = (AAA, ZZZ)",
    "ZZZ, = (ZZZ, ZZZ)"
  )
