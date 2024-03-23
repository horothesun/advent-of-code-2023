//import cats.derived.*
import cats.implicits.*
import scala.math.Ordering.Implicits.infixOrderingOps

case class Time(millis: Int)

case class Distance(millimeters: Int)
object Distance:
  given Ordering[Distance] = Ordering.by(_.millimeters)

case class Race(allowance: Time, best: Distance)

def parseRaces(input: List[String]): Option[List[Race]] = input match
  case ts :: ds :: Nil =>
    (parseTimes(ts), parseDistances(ds)).flatMapN((times, distances) =>
      if (times.length != distances.length) None else Some(times.zip(distances).map(Race.apply))
    )
  case _ => None

def parseAs[T](apply: Int => T, s: String): Option[List[T]] =
  s.split(' ').toList.filterNot(_.isEmpty).traverse(_.toIntOption.map(apply))

def parseTimes(times: String): Option[List[Time]] = times.toList match
  case 'T' :: 'i' :: 'm' :: 'e' :: ':' :: ts => parseAs(Time.apply, ts.mkString)
  case _                                     => None

def parseDistances(distances: String): Option[List[Distance]] = distances.toList match
  case 'D' :: 'i' :: 's' :: 't' :: 'a' :: 'n' :: 'c' :: 'e' :: ':' :: ds => parseAs(Distance.apply, ds.mkString)
  case _                                                                 => None

def getAllPossibleDistances(allowance: Time): List[Distance] =
  List.range[Int](0, 1 + allowance.millis).map { holdMillis =>
    val raceDuration = allowance.millis - holdMillis
    val speed = holdMillis
    Distance(speed * raceDuration)
  }

def countWaysToWin(r: Race): Int = getAllPossibleDistances(r.allowance).count(_ > r.best)

def getMultipliedWaysToWin(rs: List[Race]): Int = rs.map(countWaysToWin).product

def getMultipliedWaysToWin(inputs: List[String]): Option[Int] = parseRaces(inputs).map(getMultipliedWaysToWin)
