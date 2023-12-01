import cats.implicits.*
import cats.Monoid
import scala.annotation.tailrec
import Digit.*

opaque type AmendedCalibration = String
object AmendedCalibration {
  def apply(input: String): AmendedCalibration = input
}

opaque type Calibration = Int
object Calibration {

  implicit val monoid: Monoid[Calibration] = Monoid.instance[Calibration](
    emptyValue = 0,
    cmb = (c1, c2) => Calibration((c1: Int) + (c2: Int))
  )

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

  def from(first: Int, last: Int): Calibration = Calibration(10 * first + last)

}

def getCalibrations(inputs: List[String]): Option[List[Calibration]] =
  inputs.map(AmendedCalibration.apply).traverse(Calibration.from)

def getCalibrationsSum(inputs: List[String]): Option[Calibration] = getCalibrations(inputs).map(_.combineAll)

enum Digit derives CanEqual {
  case One, Two, Three, Four, Five, Six, Seven, Eight, Nine

  def toInt: Int = this match
    case One   => 1
    case Two   => 2
    case Three => 3
    case Four  => 4
    case Five  => 5
    case Six   => 6
    case Seven => 7
    case Eight => 8
    case Nine  => 9
}
object Digit                {
  def fromPrefixOf(s: String): (Option[Digit], String) =
    val (optDigit, restChars) = s.toList match {
      case '1' :: cs                             => (Some(One), cs)
      case '2' :: cs                             => (Some(Two), cs)
      case '3' :: cs                             => (Some(Three), cs)
      case '4' :: cs                             => (Some(Four), cs)
      case '5' :: cs                             => (Some(Five), cs)
      case '6' :: cs                             => (Some(Six), cs)
      case '7' :: cs                             => (Some(Seven), cs)
      case '8' :: cs                             => (Some(Eight), cs)
      case '9' :: cs                             => (Some(Nine), cs)
      case 'o' :: 'n' :: 'e' :: cs               => (Some(One), cs)
      case 't' :: 'w' :: 'o' :: cs               => (Some(Two), cs)
      case 't' :: 'h' :: 'r' :: 'e' :: 'e' :: cs => (Some(Three), cs)
      case 'f' :: 'o' :: 'u' :: 'r' :: cs        => (Some(Four), cs)
      case 'f' :: 'i' :: 'v' :: 'e' :: cs        => (Some(Five), cs)
      case 's' :: 'i' :: 'x' :: cs               => (Some(Six), cs)
      case 's' :: 'e' :: 'v' :: 'e' :: 'n' :: cs => (Some(Seven), cs)
      case 'e' :: 'i' :: 'g' :: 'h' :: 't' :: cs => (Some(Eight), cs)
      case 'n' :: 'i' :: 'n' :: 'e' :: cs        => (Some(Nine), cs)
      case cs @ _                                => (None, cs)
    }
    (optDigit, restChars.mkString)
}

def getDigits(input: String): List[Digit] =
  @tailrec
  def aux(acc: List[Digit], chars: List[Char]): List[Digit] =
    chars match
      case Nil     => acc
      case _ :: cs =>
        val (optDigit, rest)  = Digit.fromPrefixOf(chars.mkString)
        val (newAcc, newRest) = optDigit.fold(ifEmpty = (acc, cs))(d => (d :: acc, rest.toList))
        aux(newAcc, newRest)
  aux(List.empty, input.toList).reverse

def getFancyCalibrations(inputs: List[String]): Option[List[Calibration]] =
  inputs.map(getDigits).traverse(Calibration.from)

def getFancyCalibrationsSum(inputs: List[String]): Option[Calibration] = getFancyCalibrations(inputs).map(_.combineAll)
