import cats.*
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec
import Day11.Image.*
import Day11.NonEmptyMatrix.*
import Day11.Pixel.*
import Day11.Space.*

object Day11:

  case class Pos(row: Int, col: Int):
    def manhattanDistanceTo(that: Pos): Int = Math.abs(row - that.row) + Math.abs(col - that.col)

  case class NonEmptyMatrix[A](rows: NonEmptyList[NonEmptyList[A]]) derives Functor, Foldable:

    lazy val width: Int = topRow.length
    lazy val height: Int = rows.length

    lazy val topRow: NonEmptyList[A] = rows.head
    lazy val leftCol: NonEmptyList[A] = transposed.topRow

    lazy val rotatedCW: NonEmptyMatrix[A] = transposed.vFlipped
    lazy val rotatedCCW: NonEmptyMatrix[A] = transposed.hFlipped
    lazy val transposed: NonEmptyMatrix[A] = NonEmptyMatrix(transpose(rows))
    lazy val vFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.map(_.reverse))
    lazy val hFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.reverse)

    lazy val zipWithPos: NonEmptyMatrix[(A, Pos)] = NonEmptyMatrix(
      rows.zipWithIndex.map((r, rowId) => r.zipWithIndex.map((a, colId) => (a, Pos(rowId, colId))))
    )

    def collect[B](pf: PartialFunction[A, B]): List[B] = this.toList.collect(pf)

    def zip[B](that: NonEmptyMatrix[B]): NonEmptyMatrix[(A, B)] =
      NonEmptyMatrix(rows.zip(that.rows).map((r1, r2) => r1.zip(r2)))

    def focused(p1: Pos, p2: Pos): Option[NonEmptyMatrix[A]] =
      val minRow = Math.min(p1.row, p2.row)
      val maxRow = Math.max(p1.row, p2.row)
      val minCol = Math.min(p1.col, p2.col)
      val maxCol = Math.max(p1.col, p2.col)
      rows.toList
        .dropRight(height - 1 - maxRow)
        .map(_.toList.dropRight(width - 1 - maxCol))
        .drop(minRow)
        .map(_.drop(minCol))
        .traverse(_.toNel)
        .flatMap(_.toNel)
        .map(NonEmptyMatrix.apply)

  object NonEmptyMatrix:

    def apply[A](head: NonEmptyList[A], tail: NonEmptyList[A]*): NonEmptyMatrix[A] =
      NonEmptyMatrix(rows = NonEmptyList(head, tail.toList))

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

  case class Stretch(horizontal: Space, vertical: Space)

  enum Space:
    case Normal, Expanded

    def toInt(factor: Int): Int = this match
      case Normal   => 1
      case Expanded => factor

  enum Pixel:
    case EmptySpace, Galaxy
  object Pixel:
    def parse(c: Char): Option[Pixel] =
      c match
        case '.' => Some(EmptySpace)
        case '#' => Some(Galaxy)
        case _   => None

  case class Image(pixels: NonEmptyMatrix[Pixel]):

    lazy val rotatedCCW: Image = Image(pixels.rotatedCCW)

    def expanded: Image =
      val expandedRows = duplicateAllEmptySpaceRows(pixels)
      val expandedRowsAndColumns = duplicateAllEmptySpaceRows(expandedRows.rotatedCW).rotatedCCW
      Image(expandedRowsAndColumns)

    lazy val allGalaxies: Set[Pos] = pixels.zipWithPos.collect { case (Galaxy, pos) => pos }.toSet

    lazy val stretches: NonEmptyMatrix[Stretch] = hSpaces.zip(vSpaces).map(Stretch.apply)
    lazy val hSpaces: NonEmptyMatrix[Space] = rotatedCCW.vSpaces.rotatedCW
    lazy val vSpaces: NonEmptyMatrix[Space] = NonEmptyMatrix(
      pixels.rows.map { r =>
        val space = if (r.forall(_ == EmptySpace)) Expanded else Normal
        r.map(_ => space)
      }
    )

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

  def sumAllUniqueDistancesBetweenGalaxies(input: List[String]): Option[Long] = Image.parse(input).map { image =>
    allUniquePairs(image.expanded.allGalaxies).toList
      .map((p1, p2) => p1.manhattanDistanceTo(p2))
      .sum
  }

  def manhattanDistance(factor: Int, ss: NonEmptyMatrix[Stretch], src: Pos, dest: Pos): Option[Long] =
    ss.focused(src, dest).map { f =>
      val hs = f.topRow.map(_.horizontal.toInt(factor).toLong).tail
      val vs = f.leftCol.map(_.vertical.toInt(factor).toLong).tail
      (hs ++ vs).sum
    }

  def sumAllUniqueDistancesBetweenGalaxies(factor: Int, input: List[String]): Option[Long] =
    for {
      image <- Image.parse(input)
      allDistances <-
        allUniquePairs(image.allGalaxies).toList.traverse((src, dest) =>
          manhattanDistance(factor, image.stretches, src, dest)
        )
    } yield allDistances.sum
