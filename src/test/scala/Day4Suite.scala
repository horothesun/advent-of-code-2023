import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day4Suite.*

class Day4Suite extends ScalaCheckSuite:

  test("consecutiveDeduped(List(1, 2, 2, 3, 4, 2, 5), toDedupe = 2) == List(1, 2, 3, 4, 2, 5)") {
    assertEquals(consecutiveDeduped(List(1, 2, 2, 3, 4, 2, 5), toDedupe = 2), List(1, 2, 3, 4, 2, 5))
  }

  property("consecutiveDeduped(n_times_c, toDedupe = c).length == 1") {
    forAll(Gen.chooseNum(1, 100), Gen.alphaChar) { (n, c) =>
      assertEquals(consecutiveDeduped(List.fill(n)(c), toDedupe = c).length, 1)
    }
  }

  property("consecutiveDeduped(n_numChars ++ x_times_c ++ m_numChars, toDedupe = c).length == n + 1 + m") {
    forAll(
      Gen.chooseNum(1, 100),
      Gen.chooseNum(1, 100),
      Gen.chooseNum(1, 100),
      Gen.numChar,
      Gen.alphaChar
    ) { (n, x, m, num, c) =>
      val chars = List.fill(n)(num) ++ List.fill(x)(c) ++ List.fill(m)(num)
      assertEquals(consecutiveDeduped(chars, toDedupe = c).length, n + 1 + m)
    }
  }

  property("consecutiveDeduped-ing twice returns the same list") {
    forAll(Gen.listOf(Gen.chooseNum(0, 9))) { ns =>
      val firstRes = consecutiveDeduped(ns, 2)
      assertEquals(consecutiveDeduped(firstRes, 2), firstRes)
    }
  }

  test("whitespaceMerged(\"a  b c   def g  h\") == \"a b c def g h\"") {
    assertEquals(whitespaceMerged("a  b c   def g  h"), "a b c def g h")
  }

  test("whitespaceMerged(\" abc   \") == \" abc \"") {
    assertEquals(whitespaceMerged(" abc   "), " abc ")
  }

  test("whitespaceMerged(\"   abc \") == \" abc \"") {
    assertEquals(whitespaceMerged("   abc "), " abc ")
  }

  test("whitespaceMerged(\"   abc   \") == \" abc \"") {
    assertEquals(whitespaceMerged("   abc   "), " abc ")
  }

  test("CardId.from(\"Card A\") == None") {
    assertEquals(CardId.from("Card A"), None)
  }

  test("CardId.from(\"Card1\") == None") {
    assertEquals(CardId.from("Card1"), None)
  }

  test("CardId.from(\"Card 1\") == Some(CardId(1))") {
    assertEquals(CardId.from("Card 1"), Some(CardId(1)))
  }

  test("CardId.from(\"Card   42\") == Some(CardId(42))") {
    assertEquals(CardId.from("Card   42"), Some(CardId(42)))
  }

  test("smallInput parsed successfully") {
    assertEquals(
      getCards(
        List(
          "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
          "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
          "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
          "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
          "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
          "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
        )
      ),
      Some(smallDeck)
    )
  }

  test("Card 1 matches are 83, 86, 17 and 48") {
    assertEquals(card1.getMatches, List(83, 86, 17, 48))
  }

  test("Card 1 points are 8") {
    assertEquals(getPoints(matches = List(83, 86, 17, 48)), 8L)
  }

  test("smallDeck total points are 13") {
    assertEquals(getTotalPoints(smallDeck), 13L)
  }

  test("bigInput total points are 25_010") {
    assertEquals(getTotalPoints(bigInput), Some(25_010L))
  }

  test("Card 1 generates one copy of each card 2, 3, 4 and 5") {
    assertEquals(getNewCardIds(card1), List(CardId(2), CardId(3), CardId(4), CardId(5)))
  }

  test("Card 2 generates one copy of each card 3 and 4") {
    assertEquals(getNewCardIds(card2), List(CardId(3), CardId(4)))
  }

  test("Card 3 generates one copy of each card 4 and 5") {
    assertEquals(getNewCardIds(card3), List(CardId(4), CardId(5)))
  }

  test("Card 4 generates one copy of card 5") {
    assertEquals(getNewCardIds(card4), List(CardId(5)))
  }

  test("Card 5 generates no copies of any other card") {
    assertEquals(getNewCardIds(card5), List.empty[CardId])
  }

  test("Card 6 generates no copies of any other card") {
    assertEquals(getNewCardIds(card6), List.empty[CardId])
  }

  test("smallDeck generates the right number of card instances") {
    assertEquals(
      getTotalCardInstances(smallDeck),
      Map(
        CardId(1) -> 1,
        CardId(2) -> 2,
        CardId(3) -> 4,
        CardId(4) -> 8,
        CardId(5) -> 14,
        CardId(6) -> 1
      )
    )
  }

  test("smallDeck generates a total of 30 card instances") {
    assertEquals(getTotalCards(smallDeck), 30)
  }

  test("bigInput generates a total of 9_924_412 card instances") {
    assertEquals(getTotalCards(bigInput), Some(9_924_412))
  }

object Day4Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day4_input.txt")

  val card1: Card = Card(CardId(1), winning = List(41, 48, 83, 86, 17), current = List(83, 86, 6, 31, 17, 9, 48, 53))
  val card2: Card = Card(CardId(2), winning = List(13, 32, 20, 16, 61), current = List(61, 30, 68, 82, 17, 32, 24, 19))
  val card3: Card = Card(CardId(3), winning = List(1, 21, 53, 59, 44), current = List(69, 82, 63, 72, 16, 21, 14, 1))
  val card4: Card = Card(CardId(4), winning = List(41, 92, 73, 84, 69), current = List(59, 84, 76, 51, 58, 5, 54, 83))
  val card5: Card = Card(CardId(5), winning = List(87, 83, 26, 28, 32), current = List(88, 30, 70, 12, 93, 22, 82, 36))
  val card6: Card = Card(CardId(6), winning = List(31, 18, 13, 56, 72), current = List(74, 77, 10, 23, 35, 67, 36, 11))

  val smallDeck: List[Card] = List(card1, card2, card3, card4, card5, card6)
