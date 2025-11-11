import cats.data.NonEmptyList
import cats.syntax.all.*

object Day09:

  case class History(values: NonEmptyList[Long])
  object History:
    def parse(s: String): Option[History] = s.split(' ').toList match
      case Nil    => None
      case vs @ _ => vs.traverse(_.toLongOption).flatMap(_.toNel.map(History.apply))

  def parse(inputs: NonEmptyList[String]): Option[NonEmptyList[History]] = inputs.traverse(History.parse)

  def allDifferences(h: History): List[NonEmptyList[Long]] = allReversedDifferences(h).map(_.reverse)

  def allReversedDifferences(h: History): List[NonEmptyList[Long]] =
    List.unfold[NonEmptyList[Long], NonEmptyList[Long]](init = h.values.reverse) { reversed =>
      reversedDifferences(reversed).flatMap(diffs => if reversed.forall(_ == 0) then None else Some((diffs, diffs)))
    }

  def differences(vs: NonEmptyList[Long]): Option[NonEmptyList[Long]] = reversedDifferences(vs.reverse).map(_.reverse)

  def reversedDifferences(reversed: NonEmptyList[Long]): Option[NonEmptyList[Long]] =
    reversed.toList.zip(reversed.tail).map((v1, v2) => v1 - v2).toNel

  def allPredictions(h: History): List[Long] =
    val triangle = NonEmptyList(h.values.reverse, allReversedDifferences(h)).reverse
    val firstPred = if triangle.head.forall(_ == 0) then 0 else triangle.head.head
    triangle
      .foldLeft[(List[Long], Long)]((List.empty, firstPred)) { case ((preds, lastPred), revDiffs) =>
        val newPred = revDiffs.head + lastPred
        (newPred :: preds, newPred)
      }
      ._1

  def prediction(h: History): Option[Long] = allPredictions(h).headOption

  def allPredictionsSum(inputs: NonEmptyList[String]): Option[Long] =
    parse(inputs).flatMap(hs => hs.traverse(prediction)).map(_.sumAll)

  def allPastPredictions(h: History): List[Long] =
    val triangle = NonEmptyList(h.values, allDifferences(h)).reverse
    val lastPred = if triangle.head.forall(_ == 0) then 0 else triangle.head.head
    triangle
      .foldLeft[(List[Long], Long)]((List.empty, lastPred)) { case ((preds, firstPred), revDiffs) =>
        val pastPred = revDiffs.head - firstPred
        (pastPred :: preds, pastPred)
      }
      ._1

  def pastPrediction(h: History): Option[Long] = allPastPredictions(h).headOption

  def allPastPredictionsSum(inputs: NonEmptyList[String]): Option[Long] =
    parse(inputs).flatMap(hs => hs.traverse(pastPrediction)).map(_.sumAll)
