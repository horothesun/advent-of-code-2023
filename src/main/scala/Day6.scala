import cats.implicits.*
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.math.Numeric.Implicits.infixNumericOps

case class Time(millis: Long):
  def inc: Time = Time(millis + 1)
  def dec: Time = Time(millis - 1)

object Time:
  given Numeric[Time] = Numeric[Long].imap(Time.apply)(_.millis)

case class Distance(millimeters: Long)
object Distance:
  given Ordering[Distance] = Ordering.by(_.millimeters)

case class Race(allowance: Time, record: Distance)

def getTravelledDistance(allowance: Time, hold: Time): Distance =
  val raceDuration = allowance - hold
  val speed = hold.millis
  Distance(speed * raceDuration.millis)

def getTotalOutcomes(allowance: Time): Long = 1 + allowance.millis

def getAllPossibleDistances(allowance: Time): List[Distance] =
  List
    .range[Long](0, getTotalOutcomes(allowance))
    .map(holdMillis => getTravelledDistance(allowance, hold = Time(holdMillis)))

def countWaysToWin_bruteForce(race: Race): Long = getAllPossibleDistances(race.allowance).count(_ > race.record)

def getMultipliedWaysToWin_bruteForce(rs: List[Race]): Long = rs.map(countWaysToWin_bruteForce).product

def countWaysToLoseLeft(race: Race): Long = countWaysToLose(
  startHold = Numeric[Time].zero,
  isHoldInBounds = _ <= race.allowance,
  nextHold = _.inc,
  race
)

def countWaysToLoseRight(race: Race): Long = countWaysToLose(
  startHold = race.allowance,
  isHoldInBounds = _ >= Numeric[Time].zero,
  nextHold = _.dec,
  race
)

def countWaysToLose(startHold: Time, isHoldInBounds: Time => Boolean, nextHold: Time => Time, race: Race): Long =
  List
    .unfold[Distance, Time](startHold) { hold =>
      if (isHoldInBounds(hold))
        val travelled = getTravelledDistance(race.allowance, hold)
        if (race.record >= travelled) Some((travelled, nextHold(hold))) else None
      else None
    }
    .length

def countWaysToWin(race: Race): Long =
  val totalOutcomes = getTotalOutcomes(race.allowance)
  val lossesFromLeft = countWaysToLoseLeft(race)
  val allLosses = lossesFromLeft + (if (lossesFromLeft == totalOutcomes) 0 else countWaysToLoseRight(race))
  totalOutcomes - allLosses