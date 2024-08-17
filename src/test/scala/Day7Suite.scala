import cats.syntax.all.*
import munit.ScalaCheckSuite
import Day7.*
import Day7.CardLabel.*
import Day7.HandType.*
import Day7Suite.*

class Day7Suite extends ScalaCheckSuite:

  // part 1

  test("`A` card label has maximum strength"):
    assertEquals(CardLabel.values.max, A)

  test("`2` card label has minimum strength"):
    assertEquals(CardLabel.values.min, `2`)

  test("five-of-a-kind hand type has maximum strength"):
    assertEquals(HandType.values.max, FiveOfAKind)

  test("high-card hand type has minimum strength"):
    assertEquals(HandType.values.min, HighCard)

  test("AAAAA hand is five-of-a-kind"):
    assertEquals(Hand(A, A, A, A, A).handType, FiveOfAKind)

  test("AA8AA hand is four-of-a-kind"):
    assertEquals(Hand(A, A, `8`, A, A).handType, FourOfAKind)

  test("23332 hand is full-house"):
    assertEquals(Hand(`2`, `3`, `3`, `3`, `2`).handType, FullHouse)

  test("TTT98 hand is three-of-a-kind"):
    assertEquals(Hand(T, T, T, `9`, `8`).handType, ThreeOfAKind)

  test("23432 hand is two-pairs"):
    assertEquals(Hand(`2`, `3`, `4`, `3`, `2`).handType, TwoPairs)

  test("A23A4 hand is one-pair"):
    assertEquals(Hand(A, `2`, `3`, A, `4`).handType, OnePair)

  test("23456 hand is high-card"):
    assertEquals(Hand(`2`, `3`, `4`, `5`, `6`).handType, HighCard)

  test("33332 is stronger than 2AAAA"):
    assert(Hand(`3`, `3`, `3`, `3`, `2`) > Hand(`2`, A, A, A, A))

  test("2AAAA is less strong than 33332"):
    assert(Hand(`2`, A, A, A, A) < Hand(`3`, `3`, `3`, `3`, `2`))

  test("77888 is stronger than 77788"):
    assert(Hand(`7`, `7`, `8`, `8`, `8`) > Hand(`7`, `7`, `7`, `8`, `8`))

  test("77788 is less strong than 77888"):
    assert(Hand(`7`, `7`, `7`, `8`, `8`) < Hand(`7`, `7`, `8`, `8`, `8`))

  test("CardLabel.parse('K') is `K`"):
    assertEquals(CardLabel.parse('K'), Some(K))

  test("CardLabel.parse('$') is not defined"):
    assertEquals(CardLabel.parse('$'), None)

  test("Hand.parse(\"TTT98\") is well defined"):
    assertEquals(Hand.parse("TTT98"), Some(Hand(T, T, T, `9`, `8`)))

  test("parse \"32T3K 765\" input line"):
    assertEquals(Day7.parse("32T3K 765"), Some(Hand(`3`, `2`, T, `3`, K), Bid(765)))

  test("big input hands are all distinct"):
    val distinctHandsCount = Day7.parse(bigInput).map(_.groupBy((h, _) => h).keys.size)
    assertEquals(distinctHandsCount, Some(bigInput.length))

  test("small input total winnings are 6_440"):
    assertEquals(getTotalWinnings(smallInput), Some(Win(6_440)))

  test("big input total winnings are 249_748_283"):
    assertEquals(getTotalWinnings(bigInput), Some(Win(249_748_283)))

  // part 2

  test("32T3K hand is one-pair (new J rule)"):
    import CardLabelJ.*
    assertEquals(HandJ(`3`, `2`, T, `3`, K).handType, OnePair)

  test("T55J5 hand is four-of-a-kind (new J rule)"):
    import CardLabelJ.*
    assertEquals(HandJ(T, `5`, `5`, J, `5`).handType, FourOfAKind)

  test("KK677 hand is two-pairs (new J rule)"):
    import CardLabelJ.*
    assertEquals(HandJ(K, K, `6`, `7`, `7`).handType, TwoPairs)

  test("KTJJT hand is four-of-a-kind (new J rule)"):
    import CardLabelJ.*
    assertEquals(HandJ(K, T, J, J, T).handType, FourOfAKind)

  test("QQQJA hand is four-of-a-kind (new J rule)"):
    import CardLabelJ.*
    assertEquals(HandJ(Q, Q, Q, J, A).handType, FourOfAKind)

  test("small input total winnings are 5_905 (new J rule)"):
    assertEquals(getTotalWinningsJ(smallInput), Some(Win(5_905)))

  test("big input total winnings are 248_029_057 (new J rule)"):
    assertEquals(getTotalWinningsJ(bigInput), Some(Win(248_029_057)))

object Day7Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day7_input.txt")

  val smallInput: List[String] = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )
