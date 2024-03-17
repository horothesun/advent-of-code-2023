import munit.ScalaCheckSuite
import CompleteCategoryMappings.*
import Day5Suite.*

class Day5Suite extends ScalaCheckSuite:

  test("\"50 98 2\" + \"52 50 48\" complete core mapping returns correct values") {
    val completeCoreMapping = getCompleteCoreMapping(
      List(
        CategoryMapItem(destination = 50, source = 98, length = 2),
        CategoryMapItem(destination = 52, source = 50, length = 48)
      )
    )
    assertEquals(completeCoreMapping(0), 0L)
    assertEquals(completeCoreMapping(49), 49L)
    assertEquals(completeCoreMapping(50), 52L)
    assertEquals(completeCoreMapping(51), 53L)
    assertEquals(completeCoreMapping(97), 99L)
    assertEquals(completeCoreMapping(98), 50L)
    assertEquals(completeCoreMapping(99), 51L)
    assertEquals(completeCoreMapping(100), 100L)
  }

  test("parse seeds") {
    assertEquals(
      parseSeeds("seeds: 79 14 55 13"),
      Some(List(Seed(79), Seed(14), Seed(55), Seed(13)))
    )
  }

  test("parse category map items") {
    assertEquals(
      parseCategoryMapItems(
        title = "seed-to-soil map:",
        List(
          "seed-to-soil map:",
          "50 98 2",
          "52 50 48"
        )
      ),
      Some(
        List(
          CategoryMapItem(destination = 50, source = 98, length = 2),
          CategoryMapItem(destination = 52, source = 50, length = 48)
        )
      )
    )
  }

  test("split(by = 2, List(1, 2, 3, 2, 2, 4, 5)) is List(List(1), List(3), List(), List(4, 5))") {
    assertEquals(
      split(by = 2, List(1, 2, 3, 2, 2, 4, 5)),
      List(List(1), List(3), List(), List(4, 5))
    )
  }

  test("split(by = 2, List(2, 1, 2, 3, 2, 2, 4, 5, 2)) is List(List(), List(1), List(3), List(), List(4, 5), List())") {
    assertEquals(
      split(by = 2, List(2, 1, 2, 3, 2, 2, 4, 5, 2)),
      List(List(), List(1), List(3), List(), List(4, 5), List())
    )
  }

  test("smallInput parse correctly") {
    val expectedSeeds = List(Seed(79), Seed(14), Seed(55), Seed(13))
    val expectedCategoryMappings = CategoryMappings(
      seedToSoil = List(
        CategoryMapItem(destination = 50, source = 98, length = 2),
        CategoryMapItem(destination = 52, source = 50, length = 48)
      ),
      soilToFertilizer = List(
        CategoryMapItem(destination = 0, source = 15, length = 37),
        CategoryMapItem(destination = 37, source = 52, length = 2),
        CategoryMapItem(destination = 39, source = 0, length = 15)
      ),
      fertilizerToWater = List(
        CategoryMapItem(destination = 49, source = 53, length = 8),
        CategoryMapItem(destination = 0, source = 11, length = 42),
        CategoryMapItem(destination = 42, source = 0, length = 7),
        CategoryMapItem(destination = 57, source = 7, length = 4)
      ),
      waterToLight = List(
        CategoryMapItem(destination = 88, source = 18, length = 7),
        CategoryMapItem(destination = 18, source = 25, length = 70)
      ),
      lightToTemperature = List(
        CategoryMapItem(destination = 45, source = 77, length = 23),
        CategoryMapItem(destination = 81, source = 45, length = 19),
        CategoryMapItem(destination = 68, source = 64, length = 13)
      ),
      temperatureToHumidity = List(
        CategoryMapItem(destination = 0, source = 69, length = 1),
        CategoryMapItem(destination = 1, source = 0, length = 69)
      ),
      humidityToLocation = List(
        CategoryMapItem(destination = 60, source = 56, length = 37),
        CategoryMapItem(destination = 56, source = 93, length = 4)
      )
    )
    assertEquals(parseInputs(smallInput), Some((expectedSeeds, expectedCategoryMappings)))
  }

  test("smallInput's min location is 35") {
    assertEquals(getMinLocation(smallInput), Some(Location(35)))
  }

  test("bigInput's min location is 226_172_555") {
    assertEquals(getMinLocation(bigInput), Some(Location(226_172_555)))
  }

object Day5Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day5_input.txt")

  val smallInput: List[String] = List(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  )
