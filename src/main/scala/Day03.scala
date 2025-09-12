import cats.Traverse
import cats.derived.*
import cats.syntax.all.*

object Day03:

  val DOT_CHAR = '.'

  case class Symbol(c: Char)
  object Symbol:
    def from(c: Char): Option[Symbol] = if c == DOT_CHAR || c.isDigit then None else Some(Symbol(c))

  case class PartNumber(n: Int)
  object PartNumber:
    def from(s: String): Option[PartNumber] = s.toIntOption.map(PartNumber.apply)

  case class Part(s: Symbol, pn: PartNumber)

  case class Annotated[+A](value: A, pos0Based: Int, length: Int) derives Traverse

  type Literal = Symbol | PartNumber

  type Row = List[Annotated[Literal]]

  def parseRow(s: String): Option[Row] =
    val (lastPartNumber, optLits) = s.toList.zipWithIndex
      .foldLeft[(Option[Annotated[String]], List[Option[Annotated[Literal]]])]((None, List.empty)) {
        case ((nextPartNumber, acc), (c, pos)) =>
          c match
            case DOT_CHAR =>
              nextPartNumber.fold(ifEmpty = (None, acc))(npn => (None, acc :+ npn.traverse(PartNumber.from)))
            case d if d.isDigit =>
              val annotatedDigit = Annotated(s"$d", pos, length = 1)
              nextPartNumber.fold(ifEmpty = (Some(annotatedDigit), acc)) { npn =>
                val newNpn = Annotated(s"${npn.value}$d", pos0Based = npn.pos0Based, length = 1 + npn.length)
                (Some(newNpn), acc)
              }
            case sym =>
              val symLit = Some(Annotated[Literal](Symbol(sym), pos, length = 1))
              nextPartNumber.fold(ifEmpty = (None, acc :+ symLit))(npn =>
                (None, acc :+ npn.traverse(PartNumber.from) :+ symLit)
              )
      }
    optLits.sequence.map(ls => lastPartNumber.flatMap(_.traverse(PartNumber.from)).fold(ifEmpty = ls)(ls :+ _))

  def parse(input: List[String]): Option[List[Row]] = input.traverse(parseRow)

  val annotatedSymbol: PartialFunction[Annotated[Literal], Annotated[Symbol]] =
    case Annotated(Symbol(c), p, l) => Annotated(Symbol(c), p, l)

  val annotatedPartNumber: PartialFunction[Annotated[Literal], Annotated[PartNumber]] =
    case Annotated(PartNumber(n), p, l) => Annotated(PartNumber(n), p, l)

  def getMatchingPart(apn: Annotated[PartNumber], as: Annotated[Symbol]): Option[Part] =
    if as.pos0Based >= apn.pos0Based - 1 &&
      as.pos0Based <= apn.pos0Based + apn.length
    then Some(Part(as.value, apn.value))
    else None

  def getParts(prev: Row, curr: Row, next: Row): List[Part] =
    curr.collect(annotatedPartNumber).flatMap { apn =>
      val symbols = List(prev, curr, next).flatMap(_.collect(annotatedSymbol))
      symbols.mapFilter(getMatchingPart(apn, _))
    }

  def getPrevCurrNextRowTuples(rows: List[Row]): List[(Row, Row, Row)] =
    val emptyRow = List.empty[Annotated[Literal]]
    (emptyRow :: rows)
      .zip(rows)
      .zip(rows.drop(1) :+ emptyRow)
      .map { case ((prev, curr), next) => (prev, curr, next) }

  def getParts(rows: List[Row]): List[Part] = getPrevCurrNextRowTuples(rows).flatMap(getParts.tupled)

  def getPartNumbersSum(input: List[String]): Option[Int] = parse(input).map(rows => getParts(rows).map(_.pn.n).sum)

  case class Gear(pn1: PartNumber, pn2: PartNumber):
    val ratio: Int = pn1.n * pn2.n
  object Gear:
    val SYMBOL: Symbol = Symbol('*')

  def getGears(prev: Row, curr: Row, next: Row): List[Gear] =
    curr
      .collect(annotatedSymbol)
      .collect { case as @ Annotated(Gear.SYMBOL, _, _) => as }
      .mapFilter { as =>
        val allPartNumbers = List(prev, curr, next).flatMap(_.collect(annotatedPartNumber))
        val matchingParts = allPartNumbers.mapFilter(getMatchingPart(_, as))
        matchingParts match
          case p1 :: p2 :: Nil => Some(Gear(p1.pn, p2.pn))
          case _               => None
      }

  def getGears(rows: List[Row]): List[Gear] = getPrevCurrNextRowTuples(rows).flatMap(getGears.tupled)

  def getGearsRatioSum(input: List[String]): Option[Int] = parse(input).map(rows => getGears(rows).map(_.ratio).sum)
