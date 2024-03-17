import cats.implicits.*
import scala.math.Ordering

trait Identified:
  def id: Long

trait Constructed[A]:
  def cons(id: Long): A

case class Seed(id: Long) extends Identified
object Seed:
  given Constructed[Seed] with
    def cons(id: Long): Seed = Seed(id)

case class Soil(id: Long) extends Identified
object Soil:
  given Constructed[Soil] with
    def cons(id: Long): Soil = Soil(id)

case class Fertilizer(id: Long) extends Identified
object Fertilizer:
  given Constructed[Fertilizer] with
    def cons(id: Long): Fertilizer = Fertilizer(id)

case class Water(id: Long) extends Identified
object Water:
  given Constructed[Water] with
    def cons(id: Long): Water = Water(id)

case class Light(id: Long) extends Identified
object Light:
  given Constructed[Light] with
    def cons(id: Long): Light = Light(id)

case class Temperature(id: Long) extends Identified
object Temperature:
  given Constructed[Temperature] with
    def cons(id: Long): Temperature = Temperature(id)

case class Humidity(id: Long) extends Identified
object Humidity:
  given Constructed[Humidity] with
    def cons(id: Long): Humidity = Humidity(id)

case class Location(id: Long) extends Identified
object Location:
  given Constructed[Location] with
    def cons(id: Long): Location = Location(id)

  given Ordering[Location] = Ordering.fromLessThan((l, r) => l.id < r.id)

case class CategoryMapItem(destination: Long, source: Long, length: Long)
object CategoryMapItem:
  def from(input: String): Option[CategoryMapItem] = input.split(' ').toList.traverse(_.toLongOption) match
    case Some(d :: s :: l :: Nil) => Some(CategoryMapItem(d, s, l))
    case _                        => None

case class CategoryMappings(
  seedToSoil: List[CategoryMapItem],
  soilToFertilizer: List[CategoryMapItem],
  fertilizerToWater: List[CategoryMapItem],
  waterToLight: List[CategoryMapItem],
  lightToTemperature: List[CategoryMapItem],
  temperatureToHumidity: List[CategoryMapItem],
  humidityToLocation: List[CategoryMapItem]
)

case class CompleteCategoryMappings(
  seedToSoil: Seed => Soil,
  soilToFertilizer: Soil => Fertilizer,
  fertilizerToWater: Fertilizer => Water,
  waterToLight: Water => Light,
  lightToTemperature: Light => Temperature,
  temperatureToHumidity: Temperature => Humidity,
  humidityToLocation: Humidity => Location
):
  def seedToLocation: Seed => Location =
    seedToSoil
      .andThen(soilToFertilizer)
      .andThen(fertilizerToWater)
      .andThen(waterToLight)
      .andThen(lightToTemperature)
      .andThen(temperatureToHumidity)
      .andThen(humidityToLocation)

object CompleteCategoryMappings:
  def from(cms: CategoryMappings): CompleteCategoryMappings =
    CompleteCategoryMappings(
      seedToSoil = getCompleteMapping[Seed, Soil](cms.seedToSoil),
      soilToFertilizer = getCompleteMapping[Soil, Fertilizer](cms.soilToFertilizer),
      fertilizerToWater = getCompleteMapping[Fertilizer, Water](cms.fertilizerToWater),
      waterToLight = getCompleteMapping[Water, Light](cms.waterToLight),
      lightToTemperature = getCompleteMapping[Light, Temperature](cms.lightToTemperature),
      temperatureToHumidity = getCompleteMapping[Temperature, Humidity](cms.temperatureToHumidity),
      humidityToLocation = getCompleteMapping[Humidity, Location](cms.humidityToLocation)
    )

  def getCompleteCoreMapping(cms: List[CategoryMapItem]): Long => Long = src =>
    cms
      .findLast(cm => src >= cm.source && src < cm.source + cm.length)
      .map(cm => src - cm.source + cm.destination)
      .getOrElse(src)

  def getCompleteMapping[S <: Identified, D](cms: List[CategoryMapItem])(using dest: Constructed[D]): S => D =
    val dFromS = getCompleteCoreMapping(cms)
    src => dest.cons(dFromS(src.id))

def parseSeeds(input: String): Option[List[Seed]] = input.split(": ") match
  case Array("seeds", ids) => ids.split(' ').toList.traverse(_.toLongOption.map(Seed.apply))
  case _                   => None

def parseCategoryMapItems(title: String, inputs: List[String]): Option[List[CategoryMapItem]] = inputs match
  case head :: tail if head == title => tail.traverse(CategoryMapItem.from)
  case _                             => None

def parseInputs(inputs: List[String]): Option[(List[Seed], CategoryMappings)] = split(by = "", inputs) match
  case (seeds :: Nil) ::
      seedToSoil ::
      soilToFertilizer ::
      fertilizerToWater ::
      waterToLight ::
      lightToTemperature ::
      temperatureToHumidity ::
      humidityToLocation ::
      Nil =>
    (
      parseSeeds(seeds),
      (
        parseCategoryMapItems(title = "seed-to-soil map:", seedToSoil),
        parseCategoryMapItems(title = "soil-to-fertilizer map:", soilToFertilizer),
        parseCategoryMapItems(title = "fertilizer-to-water map:", fertilizerToWater),
        parseCategoryMapItems(title = "water-to-light map:", waterToLight),
        parseCategoryMapItems(title = "light-to-temperature map:", lightToTemperature),
        parseCategoryMapItems(title = "temperature-to-humidity map:", temperatureToHumidity),
        parseCategoryMapItems(title = "humidity-to-location map:", humidityToLocation)
      ).mapN(CategoryMappings.apply)
    ).tupled
  case _ => None

def split[A](by: A, as: List[A]): List[List[A]] =
  val fakeRight = as.lastOption.map(l => if (l == by) List.empty[A] else List(by)).getOrElse(List.empty[A])
  val middle = (as ++ fakeRight)
    .foldLeft((List.empty[A], List.empty[List[A]])) { case ((group, acc), a) =>
      if (a == by) (List.empty[A], acc.appended(group)) else (group.appended(a), acc)
    }
    ._2
  val right =
    as.lastOption.map(l => if (l == by) List(List.empty[A]) else List.empty[List[A]]).getOrElse(List.empty[List[A]])
  middle ++ right

def getMinLocation(seeds: List[Seed], cms: CategoryMappings): Option[Location] =
  val seedToLocation = CompleteCategoryMappings.from(cms).seedToLocation
  seeds.map(seedToLocation).minOption

def getMinLocation(inputs: List[String]): Option[Location] =
  parseInputs(inputs).flatMap { case (seeds, cms) => getMinLocation(seeds, cms) }
