import munit.ScalaCheckSuite
import Day3.*
import Day3Suite.*

class Day3Suite extends ScalaCheckSuite:

  test("parse(smallInput) returns the correct value") {
    assertEquals(parse(smallInput), Some(smallRows))
  }

  test("parseRow(\".......321\") returns the correct value") {
    assertEquals(
      parseRow(".......321"),
      Some(List(Annotated(PartNumber(321), pos0Based = 7, length = 3)))
    )
  }

  test("parse(bigInput) is defined") {
    assert(parse(bigInput).isDefined)
  }

  test("getParts(smallRows) returns the correct Parts") {
    assertEquals(
      getParts(smallRows),
      List(
        Part(Symbol('*'), PartNumber(467)),
        Part(Symbol('*'), PartNumber(35)),
        Part(Symbol('#'), PartNumber(633)),
        Part(Symbol('*'), PartNumber(617)),
        Part(Symbol('+'), PartNumber(592)),
        Part(Symbol('*'), PartNumber(755)),
        Part(Symbol('$'), PartNumber(664)),
        Part(Symbol('*'), PartNumber(598))
      )
    )
  }

  test("getPartNumbersSum(smallInput) == Some(4361)") {
    assertEquals(getPartNumbersSum(smallInput), Some(4361))
  }

  test("getPartNumbersSum(bigInput) == Some(554_003)") {
    assertEquals(getPartNumbersSum(bigInput), Some(554_003))
  }

  test("getGearsRatioSum(smallInput) == Some(467_835)") {
    assertEquals(getGearsRatioSum(smallInput), Some(467_835))
  }

  test("getGearsRatioSum(bigInput) == Some(87_263_515)") {
    assertEquals(getGearsRatioSum(bigInput), Some(87_263_515))
  }

object Day3Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day3_input.txt")

  val smallInput: List[String] = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )
  val smallRows: List[Row] = List(
    List(
      Annotated(PartNumber(467), pos0Based = 0, length = 3),
      Annotated(PartNumber(114), pos0Based = 5, length = 3)
    ),
    List(Annotated(Symbol('*'), pos0Based = 3, length = 1)),
    List(
      Annotated(PartNumber(35), pos0Based = 2, length = 2),
      Annotated(PartNumber(633), pos0Based = 6, length = 3)
    ),
    List(Annotated(Symbol('#'), pos0Based = 6, length = 1)),
    List(
      Annotated(PartNumber(617), pos0Based = 0, length = 3),
      Annotated(Symbol('*'), pos0Based = 3, length = 1)
    ),
    List(
      Annotated(Symbol('+'), pos0Based = 5, length = 1),
      Annotated(PartNumber(58), pos0Based = 7, length = 2)
    ),
    List(Annotated(PartNumber(592), pos0Based = 2, length = 3)),
    List(Annotated(PartNumber(755), pos0Based = 6, length = 3)),
    List(
      Annotated(Symbol('$'), pos0Based = 3, length = 1),
      Annotated(Symbol('*'), pos0Based = 5, length = 1)
    ),
    List(
      Annotated(PartNumber(664), pos0Based = 1, length = 3),
      Annotated(PartNumber(598), pos0Based = 5, length = 3)
    )
  )
