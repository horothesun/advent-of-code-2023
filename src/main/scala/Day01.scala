import cats.Monoid
import cats.syntax.all.*
import scala.annotation.tailrec

object Day01:

  opaque type AmendedCalibration = String
  object AmendedCalibration:
    def apply(input: String): AmendedCalibration = input

  opaque type Calibration = Int
  object Calibration:
    given Monoid[Calibration]:
      val empty: Calibration = 0
      def combine(c1: Calibration, c2: Calibration): Calibration = c1 + c2

    def apply(i: Int): Calibration = i

    def from(ac: AmendedCalibration): Option[Calibration] =
      val chars = ac.toList
      (
        chars.find(_.isDigit).map(_.asDigit),
        chars.findLast(_.isDigit).map(_.asDigit)
      ).mapN(from)

    def from(digits: List[Digit]): Option[Calibration] =
      (
        digits.headOption.map(_.toInt),
        digits.lastOption.map(_.toInt)
      ).mapN(from)

    def from(first: Int, last: Int): Calibration = 10 * first + last

  def getCalibrations(inputs: List[String]): Option[List[Calibration]] =
    inputs.map(AmendedCalibration.apply).traverse(Calibration.from)

  def getCalibrationsSum(inputs: List[String]): Option[Calibration] = getCalibrations(inputs).map(_.combineAll)

  enum Digit:
    case One, Two, Three, Four, Five, Six, Seven, Eight, Nine

    def toInt: Int = 1 + this.ordinal

  object Digit:
    def fromDigitCharPrefix(s: String): Option[Digit] = s.toList match
      case '1' :: _ => Some(One)
      case '2' :: _ => Some(Two)
      case '3' :: _ => Some(Three)
      case '4' :: _ => Some(Four)
      case '5' :: _ => Some(Five)
      case '6' :: _ => Some(Six)
      case '7' :: _ => Some(Seven)
      case '8' :: _ => Some(Eight)
      case '9' :: _ => Some(Nine)
      case _        => None

    def fromWordPrefix(s: String): Option[Digit] = s.toList match
      case 'o' :: 'n' :: 'e' :: _               => Some(One)
      case 't' :: 'w' :: 'o' :: _               => Some(Two)
      case 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ => Some(Three)
      case 'f' :: 'o' :: 'u' :: 'r' :: _        => Some(Four)
      case 'f' :: 'i' :: 'v' :: 'e' :: _        => Some(Five)
      case 's' :: 'i' :: 'x' :: _               => Some(Six)
      case 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ => Some(Seven)
      case 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ => Some(Eight)
      case 'n' :: 'i' :: 'n' :: 'e' :: _        => Some(Nine)
      case _                                    => None

    def fromSlidingPrefixOf(s: String): (Option[Digit], String) =
      val optDigit = fromDigitCharPrefix(s).orElse(fromWordPrefix(s))
      (optDigit, optDigit.fold(ifEmpty = s)(_ => s.drop(1)))

  def getDigits(input: String): List[Digit] =
    @tailrec
    def aux(acc: List[Digit], s: String): List[Digit] = s.toList match
      case Nil     => acc
      case _ :: cs =>
        val (optDigit, rest) = Digit.fromSlidingPrefixOf(s)
        val (newAcc, newRest) = optDigit.fold(ifEmpty = (acc, cs.mkString))(d => (d :: acc, rest))
        aux(newAcc, newRest)
    aux(List.empty, input).reverse

  def getFancyCalibrations(inputs: List[String]): Option[List[Calibration]] =
    inputs.map(getDigits).traverse(Calibration.from)

  def getFancyCalibrationsSum(inputs: List[String]): Option[Calibration] =
    getFancyCalibrations(inputs).map(_.combineAll)
