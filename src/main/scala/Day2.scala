import cats.implicits.*
import cats.Monoid
import Color.*
import Validity.*

opaque type GameId = Int
object GameId:
  def apply(i: Int): GameId = i
  def from(s: String): Option[GameId] = s.split(" ") match
    case Array("Game", id) => id.toIntOption.map(GameId.apply)
    case _                 => None

enum Color:
  case Red, Green, Blue
object Color:
  def from(s: String): Option[Color] = s match
    case "red"   => Some(Red)
    case "green" => Some(Green)
    case "blue"  => Some(Blue)
    case _       => None

enum Validity:
  case Possible, Impossible

object Validity:
  given Monoid[Validity] with
    def empty: Validity = Possible
    def combine(x: Validity, y: Validity): Validity = if (x == Impossible || y == Impossible) Impossible else Possible

case class Bag(cubesByColor: Map[Color, Int]):
  def getCubes(c: Color): Int = cubesByColor.getOrElse(c, 0)

  def power: Int = Color.values.map(getCubes).toList.product

object Bag:
  val empty: Bag = Bag(cubesByColor = Color.values.toList.map(_ -> 0).toMap)
  val part1: Bag = Bag(cubesByColor = Map(Red -> 12, Green -> 13, Blue -> 14))

  val maxMonoid: Monoid[Bag] =
    def max(c: Color, b1: Bag, b2: Bag): Int = Math.max(b1.getCubes(c), b2.getCubes(c))
    Monoid.instance(
      emptyValue = Bag.empty,
      cmb = (b1, b2) =>
        Bag(cubesByColor =
          Map(
            Red -> max(Red, b1, b2),
            Green -> max(Green, b1, b2),
            Blue -> max(Blue, b1, b2)
          )
        )
    )

case class Reveal(cubesByColor: Map[Color, Int]):
  def getCubes(c: Color): Int = cubesByColor.getOrElse(c, 0)

  def validity(bag: Bag): Validity =
    def isPossibleByColor(c: Color): Validity =
      if (cubesByColor.getOrElse(c, 0) <= bag.cubesByColor.getOrElse(c, 0)) Possible else Impossible
    Color.values.toList.foldMap(isPossibleByColor)

  def fewestCubes: Bag = Bag(cubesByColor = Color.values.toList.map(c => c -> getCubes(c)).toMap)

object Reveal:
  def from(s: String): Option[Reveal] =
    s.split(", ")
      .toList
      .traverse(_.split(" ") match
        case Array(n, c) => (Color.from(c), n.toIntOption).tupled
        case _           => None
      )
      .map(cns => Reveal(cubesByColor = cns.toMap))

case class Game(id: GameId, reveals: List[Reveal]):
  def validity(bag: Bag): Validity = reveals.foldMap(r => r.validity(bag))

  def fewestCubes: Bag = reveals.foldMap(_.fewestCubes)(Bag.maxMonoid)

object Game:
  def from(s: String): Option[Game] =
    s.split(": ") match
      case Array(gid, rs) =>
        (
          GameId.from(gid),
          rs.split("; ").toList.traverse(Reveal.from)
        ).mapN(Game.apply)
      case _ => None

def getGames(input: List[String]): Option[List[Game]] = input.traverse(Game.from)

def getPossibleGameIdsSum(games: List[Game]): Int = games.filter(_.validity(Bag.part1) == Possible).map(_.id).sum[Int]

def getPossibleGameIdsSum(input: List[String]): Option[Int] = getGames(input).map(getPossibleGameIdsSum)

def getGameFewestCubesPowerSum(games: List[Game]): Int = games.map(_.fewestCubes.power).sum

def getGameFewestCubesPowerSum(input: List[String]): Option[Int] = getGames(input).map(getGameFewestCubesPowerSum)
