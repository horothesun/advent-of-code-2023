import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.Order
import fs2.{Pure, Stream}
import scala.util.Try
import Day08.Direction.*

object Day08:

  // part 1

  enum Direction:
    case L, R

  object Direction:
    def parse(c: Char): Option[Direction] = Try(Direction.valueOf(s"$c")).toOption

  case class NodeId(c1: Char, c2: Char, c3: Char):
    lazy val isStartGhost: Boolean = c3 == 'A'
    lazy val isFinishGhost: Boolean = c3 == 'Z'
  object NodeId:
    val start: NodeId = NodeId('A', 'A', 'A')
    val finish: NodeId = NodeId('Z', 'Z', 'Z')

    given Order[NodeId] = Order.by[NodeId, String](nId => s"${nId.c1}${nId.c2}${nId.c3}")

  case class Node(id: NodeId, left: NodeId, right: NodeId):
    def next(d: Direction): NodeId = d match
      case L => left
      case R => right

  object Node:
    def parse(s: String): Option[Node] = s.toList match
      case id1 :: id2 :: id3 :: ' ' :: '=' :: ' ' :: '(' :: l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: ')' :: Nil =>
        Some(Node(id = NodeId(id1, id2, id3), left = NodeId(l1, l2, l3), right = NodeId(r1, r2, r3)))
      case _ => None

  case class NavigationDocument(directions: NonEmptyList[Direction], nodes: NonEmptyList[Node])
  object NavigationDocument:
    def parseDirections(cs: NonEmptyList[Char]): Option[NonEmptyList[Direction]] = cs.traverse(Direction.parse)
    def parseNodes(inputs: NonEmptyList[String]): Option[NonEmptyList[Node]] = inputs.traverse(Node.parse)
    def parse(inputs: List[String]): Option[NavigationDocument] = inputs match
      case ds :: "" :: ns =>
        (
          ds.toList.toNel.flatMap(parseDirections),
          ns.toNel.flatMap(parseNodes)
        ).mapN(NavigationDocument.apply)
      case _ => None

  opaque type StepsCount = Long
  object StepsCount:
    def apply(l: Long): StepsCount = l

  def stepsCountToFinish(nd: NavigationDocument): Option[StepsCount] =
    val nodeById = nd.nodes.groupByNem(_.id).map(_.head)
    Stream
      .emits[Pure, Direction](nd.directions.toList)
      .repeat
      .scan[(NodeId, Option[StepsCount])]((NodeId.start, Some(0))) { case ((fromId, acc), d) =>
        nodeById(fromId).fold(ifEmpty = (fromId, None))(from => (from.next(d), acc.map(1 + _)))
      }
      .collectFirst {
        case (_, None)          => None
        case (NodeId.finish, c) => c
      }
      .toList
      .headOption
      .flatten

  def stepsCountToFinish(inputs: List[String]): Option[StepsCount] =
    NavigationDocument.parse(inputs).flatMap(stepsCountToFinish)

  // part 2

  def allStartNodeIdsGhost(ns: NonEmptyList[Node]): List[NodeId] = ns.map(_.id).filter(_.isStartGhost)

  def areAllFinishesGhost(nIds: List[NodeId]): Boolean = nIds.forall(_.isFinishGhost)

  def stepsCountToFinishGhost(nd: NavigationDocument): Option[StepsCount] =
    val nodeById = nd.nodes.groupByNem(_.id).map(_.head)
    Stream
      .emits[Pure, Direction](nd.directions.toList)
      .repeat
      .scan[(List[NodeId], Option[StepsCount])]((allStartNodeIdsGhost(nd.nodes), Some(0))) { case ((fromIds, acc), d) =>
        fromIds
          .traverse(nodeById.apply)
          .fold(ifEmpty = (fromIds, None))(fromNodes => (fromNodes.map(_.next(d)), acc.map(1 + _)))
      }
      .collectFirst {
        case (_, None)                              => None
        case (nIds, c) if areAllFinishesGhost(nIds) => c
      }
      .toList
      .headOption
      .flatten

  def stepsCountToFinishGhost(inputs: List[String]): Option[StepsCount] =
    NavigationDocument.parse(inputs).flatMap(stepsCountToFinishGhost)
