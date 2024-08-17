import munit.ScalaCheckSuite
import Day1.*
import Day1.Digit.*
import Day1Suite.*

class Day1Suite extends ScalaCheckSuite:

  test("Calibration.from(AmendedCalibration.from(\"1abc2\")) == Some(12)"):
    assertEquals(Calibration.from(AmendedCalibration("1abc2")), Some(Calibration(12)))

  test("getCalibrations(smallInput) returns correct value"):
    assertEquals(
      getCalibrations(
        List(
          "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"
        )
      ),
      Some(List(12, 38, 15, 77).map(Calibration.apply))
    )

  test("getCalibrationsSum(smallInput) returns Some(Calibration(142))"):
    assertEquals(
      getCalibrationsSum(
        List(
          "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"
        )
      ),
      Some(Calibration(142))
    )

  test("getCalibrationsSum(bigInput) returns Some(Calibration(54_388))"):
    assertEquals(getCalibrationsSum(bigInput), Some(Calibration(54_388)))

  test("getDigits(\"two1nine\") == List(Two, One, Nine)"):
    assertEquals(getDigits("two1nine"), List(Two, One, Nine))

  test("getDigits(\"eightwothree\") == List(Eight, Three)"):
    assertEquals(getDigits("eightwothree"), List(Eight, Two, Three))

  test("getDigits(\"abcone2threexyz\") == List(One, Two, Three)"):
    assertEquals(getDigits("abcone2threexyz"), List(One, Two, Three))

  test("getDigits(\"xtwone3four\") == List(Two, One, Three, Four)"):
    assertEquals(getDigits("xtwone3four"), List(Two, One, Three, Four))

  test("getDigits(\"4nineeightseven2\") == List(Four, Nine, Eight, Seven, Two)"):
    assertEquals(getDigits("4nineeightseven2"), List(Four, Nine, Eight, Seven, Two))

  test("getDigits(\"zoneight234\") == List(One, Eight, Two, Three, Four)"):
    assertEquals(getDigits("zoneight234"), List(One, Eight, Two, Three, Four))

  test("getDigits(\"7pqrstsixteen\") == List(Seven, Six)"):
    assertEquals(getDigits("7pqrstsixteen"), List(Seven, Six))

  test("getDigits(\"one279seven\") == List(One, Two, Seven, Nine, Seven)"):
    assertEquals(getDigits("one279seven"), List(One, Two, Seven, Nine, Seven))

  test("getDigits(\"sevenzbfvlpn7tkhmxtvgvfeightwobsb\") == List(Seven, Seven, Eight, Two)"):
    assertEquals(getDigits("sevenzbfvlpn7tkhmxtvgvfeightwobsb"), List(Seven, Seven, Eight, Two))

  test("getFancyCalibrations(mediumInput) == Some(List(29, 83, 13, 24, 42, 14, 76))"):
    assertEquals(
      getFancyCalibrations(
        List(
          "two1nine",
          "eightwothree",
          "abcone2threexyz",
          "xtwone3four",
          "4nineeightseven2",
          "zoneight234",
          "7pqrstsixteen"
        )
      ),
      Some(List(29, 83, 13, 24, 42, 14, 76).map(Calibration.apply))
    )

  test("getFancyCalibrationsSum(mediumInput) == Some(Calibration(281))"):
    assertEquals(
      getFancyCalibrationsSum(
        List(
          "two1nine",
          "eightwothree",
          "abcone2threexyz",
          "xtwone3four",
          "4nineeightseven2",
          "zoneight234",
          "7pqrstsixteen"
        )
      ),
      Some(Calibration(281))
    )

  test("getFancyCalibrationsSum(bigInput) == Some(Calibration(53_515))"):
    assertEquals(getFancyCalibrationsSum(bigInput), Some(Calibration(53_515)))

object Day1Suite:
  val bigInput: List[String] = getLinesFromFile("src/test/scala/day1_input.txt")
