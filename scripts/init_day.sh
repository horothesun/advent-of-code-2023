#!/bin/bash

AOC_DAY="$1"

[[ -z "${AOC_DAY}" ]] && echo "Error: AoC day must be passed as first argument" && exit 10

cat <<EOT >> "src/main/scala/Day${AOC_DAY}.scala"
import cats.implicits.*

def day${AOC_DAY}: Int = 42
EOT

touch "src/test/scala/day${AOC_DAY}_input.txt"

cat <<EOT >> "src/test/scala/Day${AOC_DAY}Suite.scala"
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day${AOC_DAY}Suite.*

class Day${AOC_DAY}Suite extends ScalaCheckSuite {

  test("day${AOC_DAY} == 42") {
    assertEquals(day${AOC_DAY}, 42)
  }

}
object Day${AOC_DAY}Suite {

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day${AOC_DAY}_input.txt")

}
EOT
