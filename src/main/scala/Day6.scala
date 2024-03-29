import cats.implicits.*
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.math.Numeric.Implicits.infixNumericOps

case class Time(millis: Long):
  def inc: Time = Time(millis + 1)
  def dec: Time = Time(millis - 1)

object Time:
  given Numeric[Time] = Numeric[Long].imap(Time.apply)(_.millis)
  def zero: Time = Time(0)

case class Distance(millimeters: Long)
object Distance:
  given Numeric[Distance] = Numeric[Long].imap(Distance.apply)(_.millimeters)
  given Ordering[Distance] = Ordering.by(_.millimeters)

case class Race(allowance: Time, record: Distance)

enum RaceOutcome:
  case Win
  case Loss(travelled: Distance)

import RaceOutcome.*

enum Bounds:
  case Within, Outside

def getRaceOutcome(race: Race, hold: Time): RaceOutcome =
  val travelled = getTravelledDistance(race.allowance, hold)
  if (travelled > race.record) Win else Loss(travelled)

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

def countWaysToLoseLeft(race: Race): Long =
  countWaysToLose(
    startHold = Time.zero,
    holdBounds = hold => if (hold <= race.allowance) Bounds.Within else Bounds.Outside,
    nextHold = _.inc,
    race
  )

def countWaysToLoseRight(race: Race): Long =
  countWaysToLose(
    startHold = race.allowance,
    holdBounds = hold => if (hold >= Time.zero) Bounds.Within else Bounds.Outside,
    nextHold = _.dec,
    race
  )

def countWaysToLose(startHold: Time, holdBounds: Time => Bounds, nextHold: Time => Time, race: Race): Long =
  List
    .unfold[Distance, Time](startHold) { hold =>
      holdBounds(hold) match
        case Bounds.Outside => None
        case Bounds.Within =>
          getRaceOutcome(race, hold) match
            case Win             => None
            case Loss(travelled) => Some((travelled, nextHold(hold)))
    }
    .length

def countWaysToWin(race: Race): Long =
  val totalOutcomes = getTotalOutcomes(race.allowance)
  val lossesFromLeft = countWaysToLoseLeft(race)
  val allLosses = lossesFromLeft + (if (lossesFromLeft == totalOutcomes) 0 else countWaysToLoseRight(race))
  totalOutcomes - allLosses

// recursion schemes 🧪🔬

import cats.Eval
import higherkindness.droste.*
import higherkindness.droste.data.*
import higherkindness.droste.data.list.*

type ListFAlg[A] = [B] =>> ListF[A, B]

def losingDistancesCoAlg(
  holdBounds: Time => Bounds,
  nextHold: Time => Time,
  race: Race
): Coalgebra[ListFAlg[Distance], Time] = Coalgebra { hold =>
  holdBounds(hold) match
    case Bounds.Outside => NilF
    case Bounds.Within =>
      getRaceOutcome(race, hold) match
        case Win     => NilF
        case Loss(d) => ConsF(d, nextHold(hold))
}

def losingDistancesLeftCoAlg(race: Race): Coalgebra[ListFAlg[Distance], Time] =
  losingDistancesCoAlg(
    holdBounds = hold => if (hold <= race.allowance) Bounds.Within else Bounds.Outside,
    nextHold = _.inc,
    race
  )

def losingDistancesRightCoAlg(race: Race): Coalgebra[ListFAlg[Distance], Time] =
  losingDistancesCoAlg(
    holdBounds = hold => if (hold >= Time.zero) Bounds.Within else Bounds.Outside,
    nextHold = _.dec,
    race
  )

def losingDistancesAlg: Algebra[ListFAlg[Distance], Long] = Algebra {
  case ConsF(_, tailLength) => 1 + tailLength
  case NilF                 => 0
}

def countWaysToLoseLeft_(race: Race): Long = scheme
  .hyloM(
    losingDistancesAlg.lift[Eval],
    losingDistancesLeftCoAlg(race).lift[Eval]
  )
  .apply(Time.zero)
  .value
def countWaysToLoseRight_(race: Race): Long = scheme
  .hyloM(
    losingDistancesAlg.lift[Eval],
    losingDistancesRightCoAlg(race).lift[Eval]
  )
  .apply(race.allowance)
  .value
