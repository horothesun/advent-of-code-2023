import Day10.CardinalDirection.*
import Day10.Field.*
import Day10.Inversion.*
import Day10.PipeType.*
import Day10.Tile.*
import cats.Order
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all.*
import scala.annotation.tailrec

object Day10:

  enum CardinalDirection:
    case North, South, West, East

  enum PipeType:
    case NorthAndSouth, WestAndEast, NorthAndWest, NorthAndEast, SouthAndWest, SouthAndEast

  object PipeType:
    def parse(c: Char): Option[PipeType] = c match
      case '|' => Some(NorthAndSouth)
      case '-' => Some(WestAndEast)
      case 'J' => Some(NorthAndWest)
      case 'L' => Some(NorthAndEast)
      case '7' => Some(SouthAndWest)
      case 'F' => Some(SouthAndEast)
      case _   => None

  case class CandidateConnections(north: Option[Pos], south: Option[Pos], west: Option[Pos], east: Option[Pos])
  object CandidateConnections:
    def parse(pipeType: PipeType, pos: Pos): CandidateConnections = pipeType match
      case NorthAndSouth => CandidateConnections(Some(pos.north), Some(pos.south), west = None, east = None)
      case WestAndEast   => CandidateConnections(north = None, south = None, Some(pos.west), Some(pos.east))
      case NorthAndWest  => CandidateConnections(Some(pos.north), south = None, Some(pos.west), east = None)
      case NorthAndEast  => CandidateConnections(Some(pos.north), south = None, west = None, Some(pos.east))
      case SouthAndWest  => CandidateConnections(north = None, Some(pos.south), Some(pos.west), east = None)
      case SouthAndEast  => CandidateConnections(north = None, Some(pos.south), west = None, Some(pos.east))

  case class Pos(row: Int, col: Int):
    lazy val north: Pos = Pos(row - 1, col)
    lazy val south: Pos = Pos(row + 1, col)
    lazy val west: Pos = Pos(row, col - 1)
    lazy val east: Pos = Pos(row, col + 1)

    def cardinalDirectionOf(that: Pos): Option[CardinalDirection] =
      if north == that then Some(North)
      else if south == that then Some(South)
      else if west == that then Some(West)
      else if east == that then Some(East)
      else None

  object Pos:
    given Order[Pos] = Order[String].contramap(_.toString)

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

  case class Loop(firstPath: NonEmptyList[Pos], secondPath: NonEmptyList[Pos]):

    lazy val allPositions: NonEmptySet[Pos] = firstPath.concatNel(secondPath).toNes

    lazy val startAs: Option[PipeType] =
      def nextCardinalDirection(path: NonEmptyList[Pos]): Option[CardinalDirection] =
        path.tail.headOption.flatMap(firstPath.head.cardinalDirectionOf)

      (
        nextCardinalDirection(firstPath),
        nextCardinalDirection(secondPath)
      ).flatMapN {
        case (North, South) | (South, North) => Some(NorthAndSouth)
        case (North, West) | (West, North)   => Some(NorthAndWest)
        case (North, East) | (East, North)   => Some(NorthAndEast)
        case (South, West) | (West, South)   => Some(SouthAndWest)
        case (South, East) | (East, South)   => Some(SouthAndEast)
        case (West, East) | (East, West)     => Some(WestAndEast)
        case _                               => None
      }

  enum TileRawType:
    case OnLoop, NotOnLoop

  enum TileType:
    case InsideLoop, OutsideLoop, OnLoop

  enum Inversion:
    case Vertical, NorthToSouth, SouthToNorth

  case class Field(rows: Vector[Vector[Tile]]):
    def startPos: Option[Pos] =
      val rowIndexes = Range(start = 0, end = rows.length).toList
      val colIndexes = rows.get(0).fold(ifEmpty = List.empty[Int])(r => Range(start = 0, end = r.length).toList)
      val allPositions = rowIndexes.flatMap(row => colIndexes.map(col => Pos(row, col)))
      allPositions.collectFirst { p =>
        get(p) match
          case Some(Start) => p
      }

    def get(pos: Pos): Option[Tile] = rows.get(pos.row).flatMap(_.get(pos.col))

    def oneStepFrom(pos: Pos): Set[Pos] = get(pos).toSet.flatMap {
      case Ground         => Set.empty
      case Start          => oneStepFromStart(pos)
      case Pipe(pipeType) => oneStepFrom(pipeType, pos)
    }

    def oneStepFromStart(pos: Pos): Set[Pos] =
      List(
        oneStepNorthOfStart(pos),
        oneStepSouthOfStart(pos),
        oneStepWestOfStart(pos),
        oneStepEastOfStart(pos)
      ).mapFilter(identity).toSet

    def oneStepNorthOfStart(pos: Pos): Option[Pos] = get(pos.north).collect {
      case t @ Pipe(NorthAndSouth) => t
      case t @ Pipe(SouthAndWest)  => t
      case t @ Pipe(SouthAndEast)  => t
    }.map(_ => pos.north)
    def oneStepSouthOfStart(pos: Pos): Option[Pos] = get(pos.south).collect {
      case t @ Pipe(NorthAndSouth) => t
      case t @ Pipe(NorthAndWest)  => t
      case t @ Pipe(NorthAndEast)  => t
    }.map(_ => pos.south)
    def oneStepWestOfStart(pos: Pos): Option[Pos] = get(pos.west).collect {
      case t @ Pipe(WestAndEast)  => t
      case t @ Pipe(NorthAndEast) => t
      case t @ Pipe(SouthAndEast) => t
    }.map(_ => pos.west)
    def oneStepEastOfStart(pos: Pos): Option[Pos] = get(pos.east).collect {
      case t @ Pipe(WestAndEast)  => t
      case t @ Pipe(NorthAndWest) => t
      case t @ Pipe(SouthAndWest) => t
    }.map(_ => pos.east)

    def oneStepFrom(pipeType: PipeType, pos: Pos): Set[Pos] =
      def validTiles(optPs: Option[Pos]*): Set[Pos] =
        optPs.toList.mapFilter(optP => optP.flatMap(p => get(p).map(_ => p))).toSet
      val cc = CandidateConnections.parse(pipeType, pos)
      pipeType match
        case NorthAndSouth => validTiles(cc.north, cc.south)
        case WestAndEast   => validTiles(cc.west, cc.east)
        case NorthAndWest  => validTiles(cc.north, cc.west)
        case NorthAndEast  => validTiles(cc.north, cc.east)
        case SouthAndWest  => validTiles(cc.south, cc.west)
        case SouthAndEast  => validTiles(cc.south, cc.east)

    lazy val loop: Option[Loop] = for {
      s <- startPos
      case (fst, snd) <- exactly2Adjacent(s)
      loop <- loopPaths(
                fstLast = fst,
                fstPath = NonEmptyList.one(s),
                sndLast = snd,
                sndPath = NonEmptyList.one(s)
              )
    } yield loop

    @tailrec
    final def loopPaths(
      fstLast: Pos,
      fstPath: NonEmptyList[Pos],
      sndLast: Pos,
      sndPath: NonEmptyList[Pos]
    ): Option[Loop] =
      if fstLast == sndLast then Some(Loop(fstPath :+ fstLast, sndPath :+ sndLast))
      else
        (
          nextUnvisited(fstLast, fstPath),
          nextUnvisited(sndLast, sndPath)
        ).tupled match
          case Some((newFstLast, newSndLast)) =>
            loopPaths(newFstLast, fstPath :+ fstLast, newSndLast, sndPath :+ sndLast)
          case None => None

    def exactly2Adjacent(pos: Pos): Option[(Pos, Pos)] = oneStepFrom(pos).toList match
      case fst :: snd :: Nil => Some((fst, snd))
      case _                 => None

    def nextUnvisited(pos: Pos, visited: NonEmptyList[Pos]): Option[Pos] = exactly2Adjacent(pos).flatMap {
      case (fst, snd) =>
        List(fst, snd).filterNot(visited.toList.contains) match
          case p :: Nil => Some(p)
          case _        => None
    }

    def allTileTypes(l: Loop, startAs: PipeType): Vector[Vector[TileType]] =
      allPositions.map(_.map(tileTypeAt(l, startAs, _)))

    def allPositions: Vector[Vector[Pos]] = Range(start = 0, end = rows.length).toVector
      .map(r => Range(start = 0, end = rows(r).length).map(c => Pos(r, c)).toVector)

    def tileTypeAt(l: Loop, startAs: PipeType, pos: Pos): TileType =
      import TileType.*
      if l.allPositions.contains(pos) then OnLoop
      else
        val row = rows(pos.row).zipWithIndex.map { (tile, col) =>
          import TileRawType.*
          val tileRawType = if l.allPositions.contains(Pos(pos.row, col)) then OnLoop else NotOnLoop
          (tile, tileRawType)
        }
        val inversionsCount = inversionsToEast(fromCol = pos.col, startAs, row).length
        val isEven: Int => Boolean = _ % 2 == 0
        if isEven(inversionsCount) then OutsideLoop else InsideLoop

  object Field:
    def parse(input: List[String]): Option[Field] =
      input.traverse(r => r.toList.traverse(Tile.parse).map(_.toVector)).map(rs => Field(rs.toVector))

    def inversionsToEast(fromCol: Int, startAs: PipeType, row: Vector[(Tile, TileRawType)]): List[Inversion] =
      row
        .drop(fromCol)
        .mapFilter { (tile, tileRawType) =>
          import TileRawType.*
          tileRawType match
            case OnLoop    => Some(tile)
            case NotOnLoop => None
        }
        .mapFilter {
          case Ground | Pipe(WestAndEast) => None
          case Start                      => Some(startAs)
          case Pipe(pipeType)             => Some(pipeType)
        }
        .foldLeft[(List[Inversion], Option[PipeType])]((List.empty, None)) { case ((acc, open), pipeType) =>
          pipeType match
            case NorthAndSouth               => (acc :+ Vertical, None)
            case WestAndEast                 => (acc, open)
            case NorthAndEast | SouthAndEast => (acc, Some(pipeType))
            case NorthAndWest =>
              open match
                case Some(SouthAndEast) => (acc :+ SouthToNorth, None)
                case _                  => (acc, None)
            case SouthAndWest =>
              open match
                case Some(NorthAndEast) => (acc :+ NorthToSouth, None)
                case _                  => (acc, None)
        }
        ._1

  def stepsCountToFarthestInLoop(input: List[String]): Option[Int] =
    Field.parse(input).flatMap(_.loop.map(_.firstPath.length - 1))

  def countTilesInsideLoop(input: List[String]): Option[Int] =
    import TileType.*
    for {
      field <- Field.parse(input)
      loop <- field.loop
      startAs <- loop.startAs
    } yield field.allTileTypes(loop, startAs).map(_.count(_ == InsideLoop)).sum
