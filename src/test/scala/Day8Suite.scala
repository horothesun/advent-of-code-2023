import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import Day8.*
import Day8.Direction.*
import Day8Suite.*

class Day8Suite extends ScalaCheckSuite:

  // part 1

  test("parse \"AAA = (BBB, CCC)\" node"):
    assertEquals(
      Node.parse("AAA = (BBB, CCC)"),
      Some(Node(id = NodeId('A', 'A', 'A'), left = NodeId('B', 'B', 'B'), right = NodeId('C', 'C', 'C')))
    )

  test("parse \"RLLRRL\" directions"):
    assertEquals(
      NavigationDocument.parseDirections(NonEmptyList.of('R', 'L', 'L', 'R', 'R', 'L')),
      Some(NonEmptyList.of(R, L, L, R, R, L))
    )

  test("parse first small input"):
    assertEquals(NavigationDocument.parse(smallInput1), Some(smallNavigationDocument1))

  test("parse second small input"):
    assertEquals(NavigationDocument.parse(smallInput2), Some(smallNavigationDocument2))

  test("first small navigation document requires 2 steps to reach final node"):
    assertEquals(stepsCountToFinish(smallNavigationDocument1), Some(StepsCount(2)))

  test("second small navigation document requires 6 steps to reach final node"):
    assertEquals(stepsCountToFinish(smallNavigationDocument2), Some(StepsCount(6)))

  test("big input requires 17_263 steps to reach final node"):
    assertEquals(stepsCountToFinish(bigInput), Some(StepsCount(17_263)))

  // part 2

  test("third small input requires 6 steps to reach final node as a ghost"):
    val smallInput3 = List(
      "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    )
    assertEquals(stepsCountToFinishGhost(smallInput3), Some(StepsCount(6)))

//  test("big input requires ??? steps to reach final node as ghost"):
//    assertEquals(stepsCountToFinishGhost(bigInput), Some(StepsCount(???)))

object Day8Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day8_input.txt")

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

  val smallNavigationDocument1: NavigationDocument = NavigationDocument(
    directions = NonEmptyList.of(R, L),
    nodes = NonEmptyList.of(
      Node(id = NodeId('A', 'A', 'A'), left = NodeId('B', 'B', 'B'), right = NodeId('C', 'C', 'C')),
      Node(id = NodeId('B', 'B', 'B'), left = NodeId('D', 'D', 'D'), right = NodeId('E', 'E', 'E')),
      Node(id = NodeId('C', 'C', 'C'), left = NodeId('Z', 'Z', 'Z'), right = NodeId('G', 'G', 'G')),
      Node(id = NodeId('D', 'D', 'D'), left = NodeId('D', 'D', 'D'), right = NodeId('D', 'D', 'D')),
      Node(id = NodeId('E', 'E', 'E'), left = NodeId('E', 'E', 'E'), right = NodeId('E', 'E', 'E')),
      Node(id = NodeId('G', 'G', 'G'), left = NodeId('G', 'G', 'G'), right = NodeId('G', 'G', 'G')),
      Node(id = NodeId('Z', 'Z', 'Z'), left = NodeId('Z', 'Z', 'Z'), right = NodeId('Z', 'Z', 'Z'))
    )
  )

  val smallInput2: List[String] = List(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val smallNavigationDocument2: NavigationDocument = NavigationDocument(
    directions = NonEmptyList.of(L, L, R),
    nodes = NonEmptyList.of(
      Node(id = NodeId('A', 'A', 'A'), left = NodeId('B', 'B', 'B'), right = NodeId('B', 'B', 'B')),
      Node(id = NodeId('B', 'B', 'B'), left = NodeId('A', 'A', 'A'), right = NodeId('Z', 'Z', 'Z')),
      Node(id = NodeId('Z', 'Z', 'Z'), left = NodeId('Z', 'Z', 'Z'), right = NodeId('Z', 'Z', 'Z'))
    )
  )
