import Day12.ConditionsCheck.*
import cats.Functor
import cats.data.NonEmptyList
import cats.derived.*
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

  case class Row[A](values: NonEmptyList[A]) derives Functor:
    def toContiguous: NonEmptyList[Contiguous[A]] = values.tail
      .foldLeft[NonEmptyList[Contiguous[A]]](NonEmptyList.one(Contiguous(values.head, reps = 1))) { case (cs, a) =>
        if (a == cs.head.value) NonEmptyList(cs.head.inc, cs.tail) else Contiguous(a, reps = 1) :: cs
      }
      .reverse

  object Row:
    def of[A](a: A, as: A*): Row[A] = Row(NonEmptyList(a, as.toList))

  enum Condition:
    case Operational, Damaged

    def toRaw: RawCondition = this match
      case Condition.Operational => RawCondition.Operational
      case Condition.Damaged     => RawCondition.Damaged

  object Condition:
    def valuesNel: NonEmptyList[Condition] = values.toList.toNel.get

  case class ConditionRecord(rawConditions: Row[RawCondition], contiguousDamagedGroupSizes: List[Int]):

    def getAllValidArrangements: List[Row[Condition]] = getAllArrangements(rawConditions).collect { cs =>
      damagedGroupSizesCheck(cs, contiguousDamagedGroupSizes) match
        case ConditionsCheck.Match => cs
    }

  object ConditionRecord:
    def parse(s: String): Option[ConditionRecord] =
      s.split(' ') match
        case Array(rawConditions, sizes) =>
          (
            parseRawConditionRow(rawConditions),
            parseContiguousDamagedGroupSizes(sizes)
          ).mapN(ConditionRecord.apply)
        case _ => None

    def parseRawConditionRow(s: String): Option[Row[RawCondition]] =
      s.toList.toNel.flatMap(_.traverse(RawCondition.parse).map(Row.apply))

    def parseContiguousDamagedGroupSizes(s: String): Option[List[Int]] = s.split(',').toList.traverse(_.toIntOption)

  extension [A](as: List[A])
    def interleavedLeft(that: List[A]): List[A] =
      def extendedBy(n: Int, l: List[A]): List[List[A]] = l.map(_ :: Nil) ++ List.fill(Math.max(0, n))(List.empty[A])
      val fstExt = extendedBy(that.length - as.length, as)
      val sndExt = extendedBy(as.length - that.length, that)
      fstExt.zip(sndExt).flatMap((f, s) => f ++ s)

  def allCombinationsOf[A](length: Int, values: NonEmptyList[A]): List[NonEmptyList[A]] =
    // TODO: can this be tail rec?
    def aux(n: Int): NonEmptyList[NonEmptyList[A]] =
      if (n == 1) values.map(NonEmptyList.one)
      else
        val tails = aux(n - 1)
        for {
          head <- values
          tail <- tails
        } yield head :: tail
    if (length <= 0) List.empty else aux(length).toList

  enum ConditionsCheck:
    case Match, Fail

  def damagedGroupSizesCheck(conditions: Row[Condition], contiguousDamagedGroupSizes: List[Int]): ConditionsCheck =
    val damagedReps = conditions.toContiguous.collect { case Contiguous(Condition.Damaged, reps) => reps }
    if (damagedReps == contiguousDamagedGroupSizes) Match else Fail

  def getAllArrangements(rawConditions: Row[RawCondition]): NonEmptyList[Row[Condition]] =
    val contiguousRawConditions = rawConditions.toContiguous
    val allUnknownsSize = contiguousRawConditions.collect { case Contiguous(RawCondition.Unknown, reps) => reps }.sum
    val allCombos = allCombinationsOf(length = allUnknownsSize, values = Condition.valuesNel)
    allCombos
      .map(combo =>
        val NonEmptyList(crc, crcTail) = contiguousRawConditions
        val (firstComboTail, headRes) = newComboTailAndPartialRes(combo.toList, crc)
        Row(
          crcTail
            .foldLeft[(List[Condition], NonEmptyList[Condition])]((firstComboTail, headRes)) {
              case ((comboTail, acc), c) =>
                val (newComboTail, newPartRes) = newComboTailAndPartialRes(comboTail, c)
                (newComboTail, acc.concatNel(newPartRes))
            }
            ._2
        )
      )
      .toNel
      .getOrElse(toConditionNel(rawConditions.values).map(rc => NonEmptyList.one(Row(rc))).get)

  def newComboTailAndPartialRes(
    comboTail: List[Condition],
    c: Contiguous[RawCondition]
  ): (List[Condition], NonEmptyList[Condition]) = c.value match
    case RawCondition.Operational => (comboTail, Contiguous[Condition](Condition.Operational, c.reps).toNel)
    case RawCondition.Damaged     => (comboTail, Contiguous[Condition](Condition.Damaged, c.reps).toNel)
    case RawCondition.Unknown     => (comboTail.drop(c.reps), comboTail.take(c.reps).toNel.get)

  def toConditionNel(rawConditions: NonEmptyList[RawCondition]): Option[NonEmptyList[Condition]] =
    rawConditions.traverse[Option, Condition] {
      case RawCondition.Operational => Some(Condition.Operational)
      case RawCondition.Damaged     => Some(Condition.Damaged)
      case RawCondition.Unknown     => None
    }

  def parse(input: List[String]): Option[List[ConditionRecord]] = input.traverse(ConditionRecord.parse)

  def totalValidArrangements(input: List[String]): Option[Int] =
    parse(input).map(crs => crs.map(_.getAllValidArrangements.length).sum)
