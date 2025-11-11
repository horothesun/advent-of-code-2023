import Day13.HMirror.*
import Day13.NonEmptyMatrix.*
import Day13.Pattern.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.annotation.tailrec

object Day13:

  case class NonEmptyMatrix[A](rows: NonEmptyList[NonEmptyList[A]]):

    val topRow: NonEmptyList[A] = rows.head

    val height: Int = rows.length

    def rotatedCW: NonEmptyMatrix[A] = transposed.vFlipped
    def transposed: NonEmptyMatrix[A] = NonEmptyMatrix(transpose(rows))
    def vFlipped: NonEmptyMatrix[A] = NonEmptyMatrix(rows.map(_.reverse))

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
          case Nil           => acc
          case Nil :: _      => acc
          case (_ :: _) :: _ =>
            val (newAcc, newRows) =
              rows
                .traverse(_.toNel)
                .flatMap(_.toNel)
                .map(rs => (acc :+ heads(rs), tails(rs)))
                .getOrElse((acc, List.empty))
            aux(newAcc, newRows)

      aux(acc = NonEmptyList.of(heads(rows)), tails(rows))

  extension [A](as: List[A])
    def splitBy(separator: A): List[List[A]] = (as :+ separator)
      .foldLeft[(List[A], List[List[A]])]((List.empty, List.empty)) { case ((currAcc, resAcc), newA) =>
        if newA == separator then (List.empty, resAcc :+ currAcc) else (currAcc :+ newA, resAcc)
      }
      ._2

  enum Terrain:
    case Ash, Rocks

  object Terrain:
    def parse(c: Char): Option[Terrain] = c match
      case '.' => Some(Ash)
      case '#' => Some(Rocks)
      case _   => None

  enum HMirror:
    case Reflection, NoReflection

  case class Pattern(nem: NonEmptyMatrix[Terrain]):
    def reflectionTopHeights: List[Int] = reflectionTopHeightsAux(nem)
    def reflectionLeftWidths: List[Int] = reflectionTopHeightsAux(nem.rotatedCW)

  object Pattern:

    def parse(input: List[String]): Option[Pattern] = input
      .traverse(_.toList.traverse(Terrain.parse))
      .flatMap(_.traverse(_.toNel))
      .flatMap(_.toNel)
      .map(tss => Pattern(NonEmptyMatrix(tss)))

    def hMirror[A](top: NonEmptyMatrix[A], bottom: NonEmptyMatrix[A]): HMirror =
      if top.rows.reverse.zip(bottom.rows).forall((tr, br) => tr == br) then Reflection else NoReflection

    def reflectionTopHeightsAux(nem: NonEmptyMatrix[Terrain]): List[Int] =
      Range.inclusive(1, nem.height - 1).toList.mapFilter { topHeight =>
        nem.hCut(topHeight).flatMap { (top, bottom) =>
          hMirror(top, bottom) match
            case Reflection   => Some(topHeight)
            case NoReflection => None
        }
      }

  def parse(inputs: List[String]): Option[List[Pattern]] = inputs.splitBy(separator = "").traverse(Pattern.parse)

  def notesSummary(inputs: List[String]): Option[Long] =
    parse(inputs).map(_.foldMap(p => p.reflectionLeftWidths.sum + p.reflectionTopHeights.foldMap(100 * _)))
