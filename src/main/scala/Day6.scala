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

def getAllPossibleDistances(allowance: Time): List[Distance] =
  List.range[Long](0, 1 + allowance.millis).map(holdMillis => getTravelledDistance(allowance, hold = Time(holdMillis)))

def countWaysToWin_bruteForce(r: Race): Long = getAllPossibleDistances(r.allowance).count(_ > r.record)

def getMultipliedWaysToWin_bruteForce(rs: List[Race]): Long = rs.map(countWaysToWin_bruteForce).product

def waysToLoseLeft(race: Race): Long =
  waysToLose(startHold = Time(0), isHoldInBounds = _ <= race.allowance, nextHold = _.inc, race)

def waysToLoseRight(race: Race): Long =
  waysToLose(startHold = race.allowance, isHoldInBounds = _ >= Time(0), nextHold = _.dec, race)

def waysToLose(startHold: Time, isHoldInBounds: Time => Boolean, nextHold: Time => Time, race: Race): Long =
  List
    .unfold[Distance, Time](startHold) { hold =>
      if (isHoldInBounds(hold))
        val travelled = getTravelledDistance(race.allowance, hold)
        if (race.record >= travelled) Some((travelled, nextHold(hold))) else None
      else None
    }
    .length

def waysToWin(r: Race, losses: Long): Long = 1 + r.allowance.millis - losses

def countWaysToWin(r: Race): Long =
  val lossesFromLeft = waysToLoseLeft(r)
  val losses = lossesFromLeft + (if (lossesFromLeft == 1 + r.allowance.millis) 0 else waysToLoseRight(r))
  waysToWin(r, losses)
