import Day13.NonEmptyMatrix.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.annotation.tailrec

object Day13:

  case class NonEmptyMatrix[A](rows: NonEmptyList[NonEmptyList[A]]):

    val topRow: NonEmptyList[A] = rows.head

    val height: Int = rows.length

    def rotatedCCW: NonEmptyMatrix[A] = transposed.hFlipped
    def transposed: NonEmptyMatrix[A] = NonEmptyMatrix(transpose(rows))
    def hFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.reverse)

    // top & bottom NEMs given 0 < topHeight < height (=> height > 1)
    def hCut(topHeight: Int): Option[(NonEmptyMatrix[A], NonEmptyMatrix[A])] =
      val (topRows, bottomRows) = rows.toList.splitAt(topHeight)
      (topRows.toNel, bottomRows.toNel).mapN((t, b) => (NonEmptyMatrix(t), NonEmptyMatrix(b)))

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

  enum Terrain:
    case Ash, Rocks

  object Terrain:
    def parse(c: Char): Option[Terrain] = c match
      case '.' => Some(Ash)
      case '#' => Some(Rocks)
      case _   => None

  case class Field(nem: NonEmptyMatrix[Terrain])

  object Field:
    def parse(input: List[String]): Option[Field] = input
      .traverse(_.toList.traverse(Terrain.parse).flatMap(_.toNel))
      .flatMap(_.toNel)
      .map(tss => Field(NonEmptyMatrix(tss)))
