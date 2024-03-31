import cats.{Order, Semigroup, SemigroupK}
import cats.data.NonEmptyList
import cats.implicits.*
import scala.util.Try
import HandType.*

case class Strength(value: Int)
object Strength:
  given Order[Strength] = Order.by(_.value)

enum CardLabel:
  case A, K, Q, J, T, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`

  def strength: Strength = Strength(CardLabel.values.length - ordinal)

object CardLabel:
  given Order[CardLabel] = Order.by(_.strength)

  def parse(c: Char): Option[CardLabel] = Try(CardLabel.valueOf(s"$c")).toOption

enum HandType:
  case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPairs, OnePair, HighCard

  def strength: Strength = Strength(HandType.values.length - ordinal)

object HandType:
  given Order[HandType] = Order.by(_.strength)

trait Comparator[A]:
  def compare: (A, A) => Int

object Comparator:
  given SemigroupK[Comparator] = new SemigroupK[Comparator]:
    override def combineK[A](x: Comparator[A], y: Comparator[A]): Comparator[A] = new Comparator[A]:
      override def compare: (A, A) => Int = (al, ar) =>
        val xCmp = x.compare(al, ar)
        if (xCmp == 0) y.compare(al, ar) else xCmp

case class Hand(c1: CardLabel, c2: CardLabel, c3: CardLabel, c4: CardLabel, c5: CardLabel):
  lazy val handType: HandType =
    List(c1, c2, c3, c4, c5).groupByNel(identity).values.toList.map(_.length).sorted.reverse match {
      case 5 :: Nil      => FiveOfAKind
      case 4 :: _        => FourOfAKind
      case 3 :: 2 :: Nil => FullHouse
      case 3 :: _        => ThreeOfAKind
      case 2 :: 2 :: _   => TwoPairs
      case 2 :: _        => OnePair
      case _             => HighCard
    }

object Hand:
  val handTypeComparator: Comparator[Hand] = new Comparator[Hand]:
    override def compare: (Hand, Hand) => Int = (hl, hr) =>
      if (hl.handType < hr.handType) -1 else if (hl.handType == hr.handType) 0 else 1
  def nthCardComparator(nth: Hand => CardLabel): Comparator[Hand] = new Comparator[Hand]:
    override def compare: (Hand, Hand) => Int = (hl, hr) =>
      val nl = nth(hl)
      val nr = nth(hr)
      if (nl < nr) -1 else if (nl == nr) 0 else 1

  given Semigroup[Comparator[Hand]] = SemigroupK[Comparator].algebra

  given Order[Hand] = Order.from((hl, hr) =>
    NonEmptyList(
      handTypeComparator,
      List[Hand => CardLabel](_.c1, _.c2, _.c3, _.c4, _.c5).map(nthCardComparator)
    ).reduce.compare(hl, hr)
  )

  def parse(s: String): Option[Hand] =
    s.toList.traverse(CardLabel.parse).collect { case c1 :: c2 :: c3 :: c4 :: c5 :: Nil => Hand(c1, c2, c3, c4, c5) }

case class Bid(value: Int)
object Bid:
  def parse(s: String): Option[Bid] = s.toIntOption.map(Bid.apply)

case class Rank(value: Int)

case class Win(value: Long)
object Win:
  given Numeric[Win] = Numeric[Long].imap(Win.apply)(_.value)

def parseHandAndBid(s: String): Option[(Hand, Bid)] = s.split(' ') match
  case Array(h, b) => (Hand.parse(h), Bid.parse(b)).tupled
  case _           => None

def parseAllHandsAndBids(inputs: List[String]): Option[List[(Hand, Bid)]] = inputs.traverse(parseHandAndBid)

def withRanks(hbs: List[(Hand, Bid)]): List[(Hand, Bid, Rank)] =
  hbs.sortBy((h, _) => h).zipWithIndex.map { case ((h, b), i) => (h, b, Rank(1 + i)) }

def getTotalWinnings(hbs: List[(Hand, Bid)]): Win = withRanks(hbs).map((_, b, r) => Win(b.value * r.value)).sum

def getTotalWinnings(inputs: List[String]): Option[Win] = parseAllHandsAndBids(inputs).map(getTotalWinnings)
