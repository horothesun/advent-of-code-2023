import cats.implicits.*

case class Time(millis: Int)
case class Distance(millimeters: Int)

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
