package day02

import utils.Utils

val input = Utils.loadInput("day02.in")

def parse(input: String): Seq[Seq[Int]] = {
  input
    .linesIterator
    .map(_.split(" ").map(_.toInt).toSeq)
    .toSeq
}

enum Safety: 
  case Safe
  case Unsafe

enum Trend: 
  case Increasing(pairs: Seq[(Int, Int)], gradual: Boolean)
  case Decreasing(paris: Seq[(Int, Int)], gradual: Boolean)
  case None


object Trend:
  def apply(pairs: Seq[(Int, Int)]): Trend = {

    val diffs = pairs.map(_ - _).map(_.abs)
    val gradual = diffs.forall((1 to 3).contains)

    if pairs.forall(_ < _) then Increasing(pairs, gradual)
    else if pairs.forall(_ > _) then Decreasing(pairs, gradual)
    else None
  }

object Safety{
  def apply(seq: Seq[Int]): Safety = {
    val pairs = seq.zip(seq.tail)

    Trend(pairs) match
      case Trend.Increasing(pairs, gradual) =>  {
        if gradual
        then Safe
        else Unsafe
      }
      case Trend.Decreasing(pairs, gradual ) => {
        if gradual
        then Safe
        else Unsafe
      }
      case Trend.None => Unsafe  
  }

  def isSafe(safety: Safety): Boolean =
    safety match
      case Safe => true
      case Unsafe => false
    
}


def part1(input: String): Int = {
  val seq = parse(input)

  seq.map(Safety(_)).count(Safety.isSafe(_))
}

def part2(input: String): Int = {
  val seq = parse(input)
  seq.count(
    inner => inner.indices.exists(i => Safety.isSafe(Safety(inner.patch(i, Nil, 1))))
  )
}


@main def main(): Unit = {
  println(part1(input))
  println(part2(input))
}
