import munit.ScalaCheckSuite
import Color.*
import Validity.*
import Day2Suite.*

class Day2Suite extends ScalaCheckSuite {

  test("getGames(smallInput) returns correct value") {
    val smallInput = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
    assertEquals(getGames(smallInput), Some(List(game1, game2, game3, game4, game5)))
  }

  test("game1.validity(Bag.part1) == Possible") {
    assertEquals(game1.validity(Bag.part1), Possible)
  }

  test("game2.validity(Bag.part1) == Possible") {
    assertEquals(game2.validity(Bag.part1), Possible)
  }

  test("game3.validity(Bag.part1) == Impossible") {
    assertEquals(game3.validity(Bag.part1), Impossible)
  }

  test("game4.validity(Bag.part1) == Impossible") {
    assertEquals(game4.validity(Bag.part1), Impossible)
  }

  test("game5.validity(Bag.part1) == Possible") {
    assertEquals(game5.validity(Bag.part1), Possible)
  }

  test("getPossibleGameIdsSum(smallInput) == Some(8)") {
    val smallInput = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
    assertEquals(getPossibleGameIdsSum(smallInput), Some(8))
  }

  test("getPossibleGameIdsSum(bigInput) == Some(2101)") {
    assertEquals(getPossibleGameIdsSum(bigInput), Some(2101))
  }

}
object Day2Suite {

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day2_input.txt")

  val game1: Game = Game(
    GameId(1),
    reveals = List(
      Reveal(Map(Blue -> 3, Red -> 4)),
      Reveal(Map(Red -> 1, Green -> 2, Blue -> 6)),
      Reveal(Map(Green -> 2))
    )
  )
  val game2: Game = Game(
    GameId(2),
    reveals = List(
      Reveal(Map(Blue -> 1, Green -> 2)),
      Reveal(Map(Green -> 3, Blue -> 4, Red -> 1)),
      Reveal(Map(Green -> 1, Blue -> 1))
    )
  )
  val game3: Game = Game(
    GameId(3),
    reveals = List(
      Reveal(Map(Green -> 8, Blue -> 6, Red -> 20)),
      Reveal(Map(Blue -> 5, Red -> 4, Green -> 13)),
      Reveal(Map(Green -> 5, Red -> 1))
    )
  )
  val game4: Game = Game(
    GameId(4),
    reveals = List(
      Reveal(Map(Green -> 1, Red -> 3, Blue -> 6)),
      Reveal(Map(Green -> 3, Red -> 6)),
      Reveal(Map(Green -> 3, Blue -> 15, Red -> 14))
    )
  )
  val game5: Game = Game(
    GameId(5),
    reveals = List(
      Reveal(Map(Red -> 6, Blue -> 1, Green -> 3)),
      Reveal(Map(Blue -> 2, Red -> 1, Green -> 2))
    )
  )

}
