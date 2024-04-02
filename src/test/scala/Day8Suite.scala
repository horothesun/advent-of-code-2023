import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import Day8.*
import Day8.Direction.*
import Day8Suite.*

class Day8Suite extends ScalaCheckSuite:

  test("parse \"AAA = (BBB, CCC)\" node") {
    assertEquals(
      Node.parse("AAA = (BBB, CCC)"),
      Some(Node(id = NodeId("AAA"), left = NodeId("BBB"), right = NodeId("CCC")))
    )
  }

  test("parse \"RLLRRL\" directions") {
    assertEquals(
      NavigationDocument.parseDirections(NonEmptyList.of('R', 'L', 'L', 'R', 'R', 'L')),
      Some(NonEmptyList.of(R, L, L, R, R, L))
    )
  }

  test("parse small input 1") {
    assertEquals(
      NavigationDocument.parse(smallInput1),
      Some(
        NavigationDocument(
          directions = NonEmptyList.of(R, L),
          nodes = NonEmptyList.of(
            Node(id = NodeId("AAA"), left = NodeId("BBB"), right = NodeId("CCC")),
            Node(id = NodeId("BBB"), left = NodeId("DDD"), right = NodeId("EEE")),
            Node(id = NodeId("CCC"), left = NodeId("ZZZ"), right = NodeId("GGG")),
            Node(id = NodeId("DDD"), left = NodeId("DDD"), right = NodeId("DDD")),
            Node(id = NodeId("EEE"), left = NodeId("EEE"), right = NodeId("EEE")),
            Node(id = NodeId("GGG"), left = NodeId("GGG"), right = NodeId("GGG")),
            Node(id = NodeId("ZZZ"), left = NodeId("ZZZ"), right = NodeId("ZZZ"))
          )
        )
      )
    )
  }

  test("parse small input 2") {
    assertEquals(
      NavigationDocument.parse(smallInput2),
      Some(
        NavigationDocument(
          directions = NonEmptyList.of(L, L, R),
          nodes = NonEmptyList.of(
            Node(id = NodeId("AAA"), left = NodeId("BBB"), right = NodeId("BBB")),
            Node(id = NodeId("BBB"), left = NodeId("AAA"), right = NodeId("ZZZ")),
            Node(id = NodeId("ZZZ"), left = NodeId("ZZZ"), right = NodeId("ZZZ"))
          )
        )
      )
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
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )
