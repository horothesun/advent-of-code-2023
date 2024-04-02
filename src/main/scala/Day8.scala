import cats.data.NonEmptyList
import cats.implicits.*
import scala.util.Try

object Day8:

  enum Direction:
    case L, R

  object Direction:
    def parse(c: Char): Option[Direction] = Try(Direction.valueOf(s"$c")).toOption

  opaque type NodeId = String
  object NodeId:
    def apply(s: String): NodeId = s
    val start: NodeId = "AAA"
    val finish: NodeId = "ZZZ"

  case class Node(id: NodeId, left: NodeId, right: NodeId)
  object Node:
    def parse(s: String): Option[Node] = s.toList match
      case id1 :: id2 :: id3 :: ' ' :: '=' :: ' ' :: '(' :: l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: ')' :: Nil =>
        Some(Node(id = NodeId(s"$id1$id2$id3"), left = NodeId(s"$l1$l2$l3"), right = NodeId(s"$r1$r2$r3")))
      case _ => None

  case class NavigationDocument(directions: NonEmptyList[Direction], nodes: NonEmptyList[Node])

  def parseDirections(cs: NonEmptyList[Char]): Option[NonEmptyList[Direction]] = cs.traverse(Direction.parse)
  def parseNodes(inputs: NonEmptyList[String]): Option[NonEmptyList[Node]] = inputs.traverse(Node.parse)
  def parse(inputs: List[String]): Option[NavigationDocument] = inputs match
    case ds :: "" :: ns =>
      (
        ds.toList.toNel.flatMap(parseDirections),
        ns.toNel.flatMap(parseNodes)
      ).mapN(NavigationDocument.apply)
    case _ => None
