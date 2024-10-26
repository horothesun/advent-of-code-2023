import cats.data.NonEmptyList
import cats.syntax.all.*

object Day12:

  enum Condition:
    case Operational, Damaged, Unknown

  object Condition:
    def parse(c: Char): Option[Condition] = c match
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

  def parseConditionRow(input: String): Option[Row[Condition]] =
    input.toList.toNel.flatMap(_.traverse(Condition.parse).map(Row.apply))
