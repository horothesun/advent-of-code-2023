import cats.data.NonEmptyList
import cats.implicits.*

object Day8:

  enum Direction:
    case Left, Right

  object Direction:
    def parse(c: Char): Option[Direction] = c match
      case 'L' => Some(Left)
      case 'R' => Some(Right)
      case _   => None

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

  def parse(inputs: List[String]): Option[NavigationDocument] = ???
