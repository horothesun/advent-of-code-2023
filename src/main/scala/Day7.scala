import cats.{Order, Semigroup, SemigroupK}
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.util.Try
import Day7.HandType.*

object Day7:

  opaque type Strength = Int

  enum HandType:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPairs, OnePair, HighCard

    def strength: Strength = HandType.values.length - ordinal

  object HandType:
    given Order[HandType] = Order.by(_.strength)
    given Ordering[HandType] = Order[HandType].toOrdering

  trait Comparator[A]:
    def compare: (A, A) => Int
    def toOrder: Order[A] = Order.from(compare)

  object Comparator:
    given SemigroupK[Comparator] = new SemigroupK[Comparator]:
      override def combineK[A](x: Comparator[A], y: Comparator[A]): Comparator[A] = new Comparator[A]:
        override def compare: (A, A) => Int = (al, ar) =>
          val xCmp = x.compare(al, ar)
          if (xCmp == 0) y.compare(al, ar) else xCmp

  enum CardLabel:
    case A, K, Q, J, T, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`

    def strength: Strength = CardLabel.values.length - ordinal

  object CardLabel:
    given Order[CardLabel] = Order.by(_.strength)
    given Ordering[CardLabel] = Order[CardLabel].toOrdering

    def parse(c: Char): Option[CardLabel] = Try(CardLabel.valueOf(s"$c")).toOption

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

    given Order[Hand] =
      NonEmptyList(
        handTypeComparator,
        List[Hand => CardLabel](_.c1, _.c2, _.c3, _.c4, _.c5).map(nthCardComparator)
      ).reduce.toOrder
    given Ordering[Hand] = Order[Hand].toOrdering

    def parse(s: String): Option[Hand] =
      s.toList.traverse(CardLabel.parse).collect { case c1 :: c2 :: c3 :: c4 :: c5 :: Nil => Hand(c1, c2, c3, c4, c5) }

  opaque type Bid = Int
  object Bid:
    def apply(i: Int): Bid = i
    def parse(s: String): Option[Bid] = s.toIntOption

  opaque type Rank = Int
  object Rank:
    def apply(i: Int): Rank = i

  opaque type Win = Long
  object Win:
    def apply(l: Long): Win = l
    def from(b: Bid, r: Rank): Win = b * r

  def parse(input: String): Option[(Hand, Bid)] = input.split(' ') match
    case Array(h, b) => (Hand.parse(h), Bid.parse(b)).tupled
    case _           => None

  def parse(inputs: List[String]): Option[List[(Hand, Bid)]] = inputs.traverse(parse)

  def withRanks(hbs: List[(Hand, Bid)]): List[(Hand, Bid, Rank)] =
    hbs.sortBy((h, _) => h).zipWithIndex.map { case ((h, b), i) => (h, b, Rank(1 + i)) }

  def getTotalWinnings(hbs: List[(Hand, Bid)]): Win = withRanks(hbs).map((_, b, r) => Win.from(b, r)).sum

  def getTotalWinnings(inputs: List[String]): Option[Win] = parse(inputs).map(getTotalWinnings)

  // part 2

  enum CardLabelJ:
    case A, K, Q, T, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`, J

    def strength: Strength = CardLabel.values.length - ordinal

    def toCardLabel: CardLabel = this match
      case CardLabelJ.A   => CardLabel.A
      case CardLabelJ.K   => CardLabel.K
      case CardLabelJ.Q   => CardLabel.Q
      case CardLabelJ.T   => CardLabel.T
      case CardLabelJ.`9` => CardLabel.`9`
      case CardLabelJ.`8` => CardLabel.`8`
      case CardLabelJ.`7` => CardLabel.`7`
      case CardLabelJ.`6` => CardLabel.`6`
      case CardLabelJ.`5` => CardLabel.`5`
      case CardLabelJ.`4` => CardLabel.`4`
      case CardLabelJ.`3` => CardLabel.`3`
      case CardLabelJ.`2` => CardLabel.`2`
      case CardLabelJ.J   => CardLabel.J

  object CardLabelJ:
    given Order[CardLabelJ] = Order.by(_.strength)

    def from(c: CardLabel): CardLabelJ = c match
      case CardLabel.A   => CardLabelJ.A
      case CardLabel.K   => CardLabelJ.K
      case CardLabel.Q   => CardLabelJ.Q
      case CardLabel.J   => CardLabelJ.J
      case CardLabel.T   => CardLabelJ.T
      case CardLabel.`9` => CardLabelJ.`9`
      case CardLabel.`8` => CardLabelJ.`8`
      case CardLabel.`7` => CardLabelJ.`7`
      case CardLabel.`6` => CardLabelJ.`6`
      case CardLabel.`5` => CardLabelJ.`5`
      case CardLabel.`4` => CardLabelJ.`4`
      case CardLabel.`3` => CardLabelJ.`3`
      case CardLabel.`2` => CardLabelJ.`2`

  case class HandJ(c1: CardLabelJ, c2: CardLabelJ, c3: CardLabelJ, c4: CardLabelJ, c5: CardLabelJ):
    lazy val handType: HandType =
      if (containsJ)
        CardLabelJ.values.toList.map(c => replaceJs(c).toHand.handType).toNel.fold(ifEmpty = HighCard)(_.maximum)
      else toHand.handType

    def containsJ: Boolean = List(c1, c2, c3, c4, c5).contains(CardLabelJ.J)

    def replaceJs(newC: CardLabelJ): HandJ =
      val j2NewC: CardLabelJ => CardLabelJ = c => if (c == CardLabelJ.J) newC else c
      HandJ(j2NewC(c1), j2NewC(c2), j2NewC(c3), j2NewC(c4), j2NewC(c5))

    def toHand: Hand = Hand(c1.toCardLabel, c2.toCardLabel, c3.toCardLabel, c4.toCardLabel, c5.toCardLabel)

  object HandJ:
    val handTypeComparator: Comparator[HandJ] = new Comparator[HandJ]:
      override def compare: (HandJ, HandJ) => Int = (hl, hr) =>
        if (hl.handType < hr.handType) -1 else if (hl.handType == hr.handType) 0 else 1
    def nthCardComparator(nth: HandJ => CardLabelJ): Comparator[HandJ] = new Comparator[HandJ]:
      override def compare: (HandJ, HandJ) => Int = (hl, hr) =>
        val nl = nth(hl)
        val nr = nth(hr)
        if (nl < nr) -1 else if (nl == nr) 0 else 1

    given Semigroup[Comparator[HandJ]] = SemigroupK[Comparator].algebra

    given Order[HandJ] =
      NonEmptyList(
        handTypeComparator,
        List[HandJ => CardLabelJ](_.c1, _.c2, _.c3, _.c4, _.c5).map(nthCardComparator)
      ).reduce.toOrder
    given Ordering[HandJ] = Order[HandJ].toOrdering

    def parse(s: String): Option[HandJ] = Hand.parse(s).map(from)

    def from(h: Hand): HandJ = HandJ(
      CardLabelJ.from(h.c1),
      CardLabelJ.from(h.c2),
      CardLabelJ.from(h.c3),
      CardLabelJ.from(h.c4),
      CardLabelJ.from(h.c5)
    )

  def parseJ(input: String): Option[(HandJ, Bid)] = input.split(' ') match
    case Array(h, b) => (HandJ.parse(h), Bid.parse(b)).tupled
    case _           => None

  def parseJ(inputs: List[String]): Option[List[(HandJ, Bid)]] = inputs.traverse(parseJ)

  def withRanksJ(hbs: List[(HandJ, Bid)]): List[(HandJ, Bid, Rank)] =
    hbs.sortBy((h, _) => h).zipWithIndex.map { case ((h, b), i) => (h, b, Rank(1 + i)) }

  def getTotalWinningsJ(hbs: List[(HandJ, Bid)]): Win = withRanksJ(hbs).map((_, b, r) => Win.from(b, r)).sum

  def getTotalWinningsJ(inputs: List[String]): Option[Win] = parseJ(inputs).map(getTotalWinningsJ)
