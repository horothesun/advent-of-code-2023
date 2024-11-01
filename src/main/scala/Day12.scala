import cats.data.NonEmptyList
import cats.syntax.all.*

object Day12:

  enum RawCondition:
    case Operational, Damaged, Unknown

  object RawCondition:
    def parse(c: Char): Option[RawCondition] = c match
      case '.' => Some(Operational)
      case '#' => Some(Damaged)
      case '?' => Some(Unknown)
      case _   => None

  case class Contiguous[A](value: A, reps: Int):
    def inc: Contiguous[A] = Contiguous(value, 1 + reps)
    def toNel: NonEmptyList[A] = NonEmptyList(value, List.fill[A](reps - 1)(value))

  case class Row[A](values: NonEmptyList[A]):
    def toContiguous: NonEmptyList[Contiguous[A]] = values.tail
      .foldLeft[NonEmptyList[Contiguous[A]]](NonEmptyList.one(Contiguous(values.head, reps = 1))) { case (cs, a) =>
        if (a == cs.head.value) NonEmptyList(cs.head.inc, cs.tail) else Contiguous(a, reps = 1) :: cs
      }
      .reverse

  object Row:
    def of[A](a: A, as: A*): Row[A] = Row(NonEmptyList(a, as.toList))

  enum Condition:
    case Operational, Damaged

  def parseRawConditionRow(input: String): Option[Row[RawCondition]] =
    input.toList.toNel.flatMap(_.traverse(RawCondition.parse).map(Row.apply))

  extension [A](as: List[A])
    def interleavedLeft(that: List[A]): List[A] =
      def extendedBy(n: Int, l: List[A]): List[List[A]] = l.map(_ :: Nil) ++ List.fill(Math.max(0, n))(List.empty[A])
      val fstExt = extendedBy(that.length - as.length, as)
      val sndExt = extendedBy(as.length - that.length, that)
      fstExt.zip(sndExt).flatMap((f, s) => f ++ s)
