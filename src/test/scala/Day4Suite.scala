import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day4Suite.*

class Day4Suite extends ScalaCheckSuite:

  test("consecutivesDeduped(List(1, 2, 2, 3, 4, 2, 5), toDedupe = 2) == List(1, 2, 3, 4, 2, 5)") {
    assertEquals(consecutivesDeduped(List(1, 2, 2, 3, 4, 2, 5), toDedupe = 2), List(1, 2, 3, 4, 2, 5))
  }

  property("consecutivesDeduped(n_times_c, toDedupe = c).length == 1") {
    forAll(Gen.chooseNum(1, 100), Gen.alphaChar) { case (n, c) =>
      assertEquals(consecutivesDeduped(List.fill(n)(c), toDedupe = c).length, 1)
    }
  }

  property("consecutivesDeduped(n_numChars ++ x_times_c ++ m_numChars, toDedupe = c).length == n + 1 + m") {
    forAll(
      Gen.chooseNum(1, 100),
      Gen.chooseNum(1, 100),
      Gen.chooseNum(1, 100),
      Gen.numChar,
      Gen.alphaChar
    ) { case (n, x, m, num, c) =>
      val chars = List.fill(n)(num) ++ List.fill(x)(c) ++ List.fill(m)(num)
      assertEquals(consecutivesDeduped(chars, toDedupe = c).length, n + 1 + m)
    }
  }

  property("consecutivesDeduped-ing twice returns the same list") {
    forAll(Gen.listOf(Gen.chooseNum(0, 9))) { ns =>
      val firstRes = consecutivesDeduped(ns, 2)
      assertEquals(consecutivesDeduped(firstRes, 2), firstRes)
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
      Some(
        List(
          Card(CardId(1), winning = List(41, 48, 83, 86, 17), current = List(83, 86, 6, 31, 17, 9, 48, 53)),
          Card(CardId(2), winning = List(13, 32, 20, 16, 61), current = List(61, 30, 68, 82, 17, 32, 24, 19)),
          Card(CardId(3), winning = List(1, 21, 53, 59, 44), current = List(69, 82, 63, 72, 16, 21, 14, 1)),
          Card(CardId(4), winning = List(41, 92, 73, 84, 69), current = List(59, 84, 76, 51, 58, 5, 54, 83)),
          Card(CardId(5), winning = List(87, 83, 26, 28, 32), current = List(88, 30, 70, 12, 93, 22, 82, 36)),
          Card(CardId(6), winning = List(31, 18, 13, 56, 72), current = List(74, 77, 10, 23, 35, 67, 36, 11))
        )
      )
    )
  }

  test("Card 1 matches are 83, 86, 17 and 48") {
    val card1 = Card(CardId(1), winning = List(41, 48, 83, 86, 17), current = List(83, 86, 6, 31, 17, 9, 48, 53))
    assertEquals(card1.getMatches, List(83, 86, 17, 48))
  }

  test("Card 1 points are 8") {
    assertEquals(getPoints(matches = List(83, 86, 17, 48)), 8L)
  }

  test("smallInput total points are 13") {
    val cards = List(
      Card(CardId(1), winning = List(41, 48, 83, 86, 17), current = List(83, 86, 6, 31, 17, 9, 48, 53)),
      Card(CardId(2), winning = List(13, 32, 20, 16, 61), current = List(61, 30, 68, 82, 17, 32, 24, 19)),
      Card(CardId(3), winning = List(1, 21, 53, 59, 44), current = List(69, 82, 63, 72, 16, 21, 14, 1)),
      Card(CardId(4), winning = List(41, 92, 73, 84, 69), current = List(59, 84, 76, 51, 58, 5, 54, 83)),
      Card(CardId(5), winning = List(87, 83, 26, 28, 32), current = List(88, 30, 70, 12, 93, 22, 82, 36)),
      Card(CardId(6), winning = List(31, 18, 13, 56, 72), current = List(74, 77, 10, 23, 35, 67, 36, 11))
    )
    assertEquals(getTotalPoints(cards), 13L)
  }

  test("bigInput total points are 25_010") {
    assertEquals(getTotalPoints(bigInput), Some(25_010L))
  }

object Day4Suite:
  val bigInput: List[String] = getLinesFromFile("src/test/scala/day4_input.txt")
