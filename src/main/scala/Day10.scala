import cats.data.NonEmptyList
import cats.implicits.*
import scala.annotation.tailrec
import Day10.PipeType.*
import Day10.Tile.*

object Day10:

  enum PipeType:
    case Vertical, Horizontal, NorthAndWest, NorthAndEast, SouthAndWest, SouthAndEast

  object PipeType:
    def parse(c: Char): Option[PipeType] = c match
      case '|' => Some(Vertical)
      case '-' => Some(Horizontal)
      case 'J' => Some(NorthAndWest)
      case 'L' => Some(NorthAndEast)
      case '7' => Some(SouthAndWest)
      case 'F' => Some(SouthAndEast)
      case _   => None

  case class CandidateConnections(north: Option[Pos], south: Option[Pos], west: Option[Pos], east: Option[Pos])
  object CandidateConnections:
    def parse(pipeType: PipeType, pos: Pos): CandidateConnections = pipeType match
      case Vertical     => CandidateConnections(Some(pos.north), Some(pos.south), west = None, east = None)
      case Horizontal   => CandidateConnections(north = None, south = None, Some(pos.west), Some(pos.east))
      case NorthAndWest => CandidateConnections(Some(pos.north), south = None, Some(pos.west), east = None)
      case NorthAndEast => CandidateConnections(Some(pos.north), south = None, west = None, Some(pos.east))
      case SouthAndWest => CandidateConnections(north = None, Some(pos.south), Some(pos.west), east = None)
      case SouthAndEast => CandidateConnections(north = None, Some(pos.south), west = None, Some(pos.east))

  case class Pos(row: Int, col: Int):
    lazy val north: Pos = Pos(row - 1, col)
    lazy val south: Pos = Pos(row + 1, col)
    lazy val west: Pos = Pos(row, col - 1)
    lazy val east: Pos = Pos(row, col + 1)

  enum Tile:
    case Ground, Start
    case Pipe(pipeType: PipeType)

  object Tile:
    def parse(c: Char): Option[Tile] = PipeType.parse(c).map(Pipe.apply).orElse {
      c match
        case '.' => Some(Ground)
        case 'S' => Some(Start)
        case _   => None
    }

  case class Loop(firstPath: NonEmptyList[Pos], secondPath: NonEmptyList[Pos])

  case class Field(rows: Vector[Vector[Tile]]):
    def getStartPos: Option[Pos] =
      val rowIndexes = Range(start = 0, end = rows.length).toList
      val colIndexes = rows.get(0).fold(ifEmpty = List.empty[Int])(r => Range(start = 0, end = r.length).toList)
      val allPositions = rowIndexes.flatMap(row => colIndexes.map(col => Pos(row, col)))
      allPositions.collectFirst { p =>
        get(p) match
          case Some(Start) => p
      }

    def get(pos: Pos): Option[Tile] = rows.get(pos.row).flatMap(_.get(pos.col))

    def oneStepFrom(pos: Pos): Set[(Tile, Pos)] = get(pos).toSet.flatMap {
      case Ground         => Set.empty
      case Start          => oneStepFromStart(pos)
      case Pipe(pipeType) => oneStepFrom(pipeType, pos)
    }

    def oneStepFromStart(pos: Pos): Set[(Tile, Pos)] =
      List(
        oneStepNorthOfStart(pos),
        oneStepSouthOfStart(pos),
        oneStepWestOfStart(pos),
        oneStepEastOfStart(pos)
      ).mapFilter(identity).toSet

    def oneStepNorthOfStart(pos: Pos): Option[(Tile, Pos)] = get(pos.north).collect {
      case t @ Pipe(Vertical)     => t
      case t @ Pipe(SouthAndWest) => t
      case t @ Pipe(SouthAndEast) => t
    }.map((_, pos.north))
    def oneStepSouthOfStart(pos: Pos): Option[(Tile, Pos)] = get(pos.south).collect {
      case t @ Pipe(Vertical)     => t
      case t @ Pipe(NorthAndWest) => t
      case t @ Pipe(NorthAndEast) => t
    }.map((_, pos.south))
    def oneStepWestOfStart(pos: Pos): Option[(Tile, Pos)] = get(pos.west).collect {
      case t @ Pipe(Horizontal)   => t
      case t @ Pipe(NorthAndEast) => t
      case t @ Pipe(SouthAndEast) => t
    }.map((_, pos.west))
    def oneStepEastOfStart(pos: Pos): Option[(Tile, Pos)] = get(pos.east).collect {
      case t @ Pipe(Horizontal)   => t
      case t @ Pipe(NorthAndWest) => t
      case t @ Pipe(SouthAndWest) => t
    }.map((_, pos.east))

    def oneStepFrom(pipeType: PipeType, pos: Pos): Set[(Tile, Pos)] =
      def validTiles(optPs: Option[Pos]*): Set[(Tile, Pos)] =
        optPs.toList.mapFilter(optP => optP.flatMap(p => get(p).map((_, p)))).toSet
      val cc = CandidateConnections.parse(pipeType, pos)
      pipeType match
        case Vertical     => validTiles(cc.north, cc.south)
        case Horizontal   => validTiles(cc.west, cc.east)
        case NorthAndWest => validTiles(cc.north, cc.west)
        case NorthAndEast => validTiles(cc.north, cc.east)
        case SouthAndWest => validTiles(cc.south, cc.west)
        case SouthAndEast => validTiles(cc.south, cc.east)

    def getLoop: Option[Loop] = for {
      s <- getStartPos
      case (fst, snd) <- exactly2Adjacent(s)
      loop <- getLoopPaths(fstLast = fst, fstPath = NonEmptyList.one(s), sndLast = snd, sndPath = NonEmptyList.one(s))
    } yield loop

    @tailrec
    final def getLoopPaths(
      fstLast: Pos,
      fstPath: NonEmptyList[Pos],
      sndLast: Pos,
      sndPath: NonEmptyList[Pos]
    ): Option[Loop] =
      if (fstLast == sndLast) Some(Loop(fstPath :+ fstLast, sndPath :+ sndLast))
      else
        (
          nextUnvisited(fstLast, fstPath),
          nextUnvisited(sndLast, sndPath)
        ).tupled match
          case Some((newFstLast, newSndLast)) =>
            getLoopPaths(newFstLast, fstPath :+ fstLast, newSndLast, sndPath :+ sndLast)
          case None => None

    def exactly2Adjacent(pos: Pos): Option[(Pos, Pos)] = oneStepFrom(pos).toList match
      case (_, fst) :: (_, snd) :: Nil => Some((fst, snd))
      case _                           => None

    def nextUnvisited(pos: Pos, visited: NonEmptyList[Pos]): Option[Pos] = exactly2Adjacent(pos).flatMap {
      case (fst, snd) =>
        List(fst, snd).filterNot(visited.toList.contains) match
          case p :: Nil => Some(p)
          case _        => None
    }

  object Field:
    def parse(input: List[String]): Option[Field] =
      input.traverse(r => r.toList.traverse(Tile.parse).map(_.toVector)).map(rs => Field(rs.toVector))

  def stepsCountToFarthestInLoop(input: List[String]): Option[Int] =
    Field.parse(input).flatMap(f => f.getLoop.map(_.firstPath.length - 1))
