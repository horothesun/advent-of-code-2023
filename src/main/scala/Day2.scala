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

enum Validity derives CanEqual:
  case Possible, Impossible

object Validity:
  given Monoid[Validity] with
    def empty: Validity = Possible
    def combine(x: Validity, y: Validity): Validity = if (x == Impossible || y == Impossible) Impossible else Possible

case class Bag(cubesByColor: Map[Color, Int])
object Bag:
  val part1: Bag = Bag(cubesByColor = Map(Red -> 12, Green -> 13, Blue -> 14))

case class Reveal(cubesByColor: Map[Color, Int]):
  def validity(bag: Bag): Validity =
    def isPossibleByColor(c: Color): Validity =
      if (cubesByColor.getOrElse(c, 0) <= bag.cubesByColor.getOrElse(c, 0)) Possible else Impossible
    Color.values.toList.foldMap(isPossibleByColor)

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
