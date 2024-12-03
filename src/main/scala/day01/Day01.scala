package day01

import scala.io.Source
import utils.Utils

val input = Utils.loadInput("day01.in")
 
def parse(input: String): (Seq[Long], Seq[Long]) = {
  val pairs = input
    .linesIterator.map(_.split("   ").map(_.toLong))
    .toSeq

  val lefts = pairs.map(_.head).toSeq.sorted
  val rights = pairs.map(_.last).toSeq.sorted

  (lefts, rights)
}

def part1(input: String): Long = 
  val (lefts, rights) = parse(input)
  lefts
    .zip(rights)
    .map((left, right) => math.abs(left - right))
    .sum

def part2(input: String): Long =
  val (lefts, rights) = parse(input)
  lefts
    .map(left => rights.count(_ == left) * left)
    .sum


@main def betterMain(): Unit = {
  val ans1 = part1(input)
  val ans2 = part2(input)
  println(s"Answer 1: $ans1")
  println(s"Answer 2: $ans2")
}
