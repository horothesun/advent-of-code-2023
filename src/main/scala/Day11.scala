import cats.*
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec
import Day11.Image.*
import Day11.NonEmptyMatrix.*
import Day11.Pixel.*

object Day11:

  case class Pos(row: Int, col: Int):
    def manhattanDistanceTo(that: Pos): Int = Math.abs(row - that.row) + Math.abs(col - that.col)

  case class NonEmptyMatrix[A](rows: NonEmptyList[NonEmptyList[A]]) derives Functor, Foldable:

    lazy val rotatedCW: NonEmptyMatrix[A] = transposed.vFlipped
    lazy val rotatedCCW: NonEmptyMatrix[A] = transposed.hFlipped
    lazy val transposed: NonEmptyMatrix[A] = NonEmptyMatrix(transpose(rows))
    lazy val vFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.map(_.reverse))
    lazy val hFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.reverse)

    lazy val zipWithPos: NonEmptyMatrix[(A, Pos)] = NonEmptyMatrix(
      rows.zipWithIndex.map((r, rowId) => r.zipWithIndex.map((a, colId) => (a, Pos(rowId, colId))))
    )

    def collect[B](pf: PartialFunction[A, B]): List[B] = this.toList.collect(pf)

  object NonEmptyMatrix:

    def transpose[A](rows: NonEmptyList[NonEmptyList[A]]): NonEmptyList[NonEmptyList[A]] =
      def heads(rows: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] = rows.map(_.head)
      def tails(rows: NonEmptyList[NonEmptyList[A]]): List[List[A]] = rows.toList.map(_.tail)
      @tailrec
      def aux(acc: NonEmptyList[NonEmptyList[A]], rows: List[List[A]]): NonEmptyList[NonEmptyList[A]] =
        rows match
          case Nil      => acc
          case Nil :: _ => acc
          case (_ :: _) :: _ =>
            val (newAcc, newRows) =
              rows
                .traverse(_.toNel)
                .flatMap(_.toNel)
                .map(rs => (acc :+ heads(rs), tails(rs)))
                .getOrElse((acc, List.empty))
            aux(newAcc, newRows)
      aux(acc = NonEmptyList.of(heads(rows)), tails(rows))

  enum Pixel:
    case EmptySpace, Galaxy
  object Pixel:
    def parse(c: Char): Option[Pixel] =
      c match
        case '.' => Some(EmptySpace)
        case '#' => Some(Galaxy)
        case _   => None

  case class Image(pixels: NonEmptyMatrix[Pixel]):

    def expanded: Image =
      val expandedRows = duplicateAllEmptySpaceRows(pixels)
      val expandedRowsAndColumns = duplicateAllEmptySpaceRows(expandedRows.rotatedCW).rotatedCCW
      Image(expandedRowsAndColumns)

    lazy val allGalaxies: Set[Pos] = pixels.zipWithPos.collect { case (Galaxy, pos) => pos }.toSet

  object Image:

    def apply(rows: NonEmptyList[NonEmptyList[Pixel]]): Image = Image(pixels = NonEmptyMatrix(rows))

    def apply(head: NonEmptyList[Pixel], tail: NonEmptyList[Pixel]*): Image =
      Image.apply(rows = NonEmptyList(head, tail.toList))

    def parse(input: List[String]): Option[Image] =
      for {
        nelInput <- input.toNel
        rows <- nelInput.traverse(_.toList.toNel.flatMap(_.traverse(Pixel.parse)))
      } yield Image(rows)

    def duplicateAllEmptySpaceRows(ps: NonEmptyMatrix[Pixel]): NonEmptyMatrix[Pixel] = NonEmptyMatrix(
      rows = ps.rows.flatMap(r => if (r.forall(_ == EmptySpace)) NonEmptyList.of(r, r) else NonEmptyList.one(r))
    )

  def allUniquePairs[A](as: Set[A]): Set[(A, A)] =
    as.toList.combinations(2).collect { case a1 :: a2 :: Nil => (a1, a2) }.toSet
