import cats.syntax.all.*

object Day04:

  case class CardId(value: Int)
  object CardId:
    def from(s: String): Option[CardId] = whitespaceMerged(s).split(' ') match
      case Array("Card", idStr) => idStr.toIntOption.map(CardId.apply)
      case _                    => None

  def whitespaceMerged(s: String): String = consecutiveDeduped(s.toList, toDedupe = ' ').mkString

  def consecutiveDeduped[A](as: List[A], toDedupe: A): List[A] =
    as.foldLeft(List.empty[A]) { (acc, a) =>
      acc match
        case Nil                               => List(a)
        case h :: _ if h == toDedupe && h == a => acc
        case _                                 => a :: acc
    }.reverse

  case class Card(id: CardId, winning: List[Int], current: List[Int]):
    def getMatches: List[Int] = current.filter(winning.contains)

  object Card:
    def from(s: String): Option[Card] =
      s.split(':').map(_.trim) match
        case Array(cardId, winsCurrs) =>
          val parseNumbs: String => Option[List[Int]] = _.split(' ').toList.traverse(_.toIntOption)
          (
            CardId.from(cardId),
            whitespaceMerged(winsCurrs).split('|').map(_.trim) match
              case Array(wins, currs) => (parseNumbs(wins), parseNumbs(currs)).tupled
              case _                  => None
          ).mapN { case (id, (winning, current)) => Card(id, winning, current) }
        case _ => None

  def getCards(input: List[String]): Option[List[Card]] = input.traverse(Card.from)

  def getPoints(matches: List[Int]): Long = matches match
    case Nil       => 0
    case _ :: tail => intPow(2, tail.length)

  def intPow(b: Int, e: Int): Long = if (e <= 0) 1 else if (e == 1) b else b * intPow(b, e - 1)

  def getTotalPoints(cards: List[Card]): Long = cards.map(c => getPoints(c.getMatches)).sum

  def getTotalPoints(input: List[String]): Option[Long] = getCards(input).map(getTotalPoints)

  def getNewCardIds(c: Card): List[CardId] =
    val base = 1 + c.id.value
    List.range(base, base + c.getMatches.length).map(CardId.apply)

  def getTotalCardInstances(cards: List[Card]): Map[CardId, Int] =
    val initial = cards.map(c => (c.id, 1)).toMap
    cards.foldLeft(initial) { (countByCardId, c) =>
      val numbOfCs = countByCardId.getOrElse(c.id, 0)
      getNewCardIds(c).foldLeft(countByCardId)((nById, id) => nById.updatedWith(id)(_.map(numbOfCs + _)))
    }

  def getTotalCards(cards: List[Card]): Int = getTotalCardInstances(cards).toList.map((_, n) => n).sum

  def getTotalCards(input: List[String]): Option[Int] = getCards(input).map(getTotalCards)
