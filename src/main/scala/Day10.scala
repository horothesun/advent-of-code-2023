import Day10.Pipe.*
import cats.implicits.*

object Day10:

  def day10: Int = 42

  case object StartPipe
  enum Pipe:
    case Horizontal, Vertical, LeftAndTop, LeftAndBottom, RightAndTop, RightAndBottom

  case class Pos(row: Int, col: Int):
    def left: Pos = Pos(row, col - 1)
    def top: Pos = Pos(row + 1, col)
    def right: Pos = Pos(row, col + 1)
    def bottom: Pos = Pos(row - 1, col)

  case class Tile(pipe: Pipe, pos: Pos):
    def candidateConnections: (Pos, Pos) = pipe match
      case Horizontal     => (pos.left, pos.right)
      case Vertical       => (pos.top, pos.bottom)
      case LeftAndTop     => (pos.left, pos.top)
      case LeftAndBottom  => (pos.left, pos.bottom)
      case RightAndTop    => (pos.right, pos.top)
      case RightAndBottom => (pos.right, pos.bottom)

  enum FieldItem:
    case Start
    case PipeItem(pipe: Pipe)

  case class Field(rows: Vector[Vector[FieldItem]]):
    def get(pos: Pos): Option[FieldItem] = rows.get(pos.row).flatMap(_.get(pos.col))

    def allConnectedTiles(pos: Pos): List[Tile] = ???
