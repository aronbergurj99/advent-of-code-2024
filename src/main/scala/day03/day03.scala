package day03

import utils.Utils

val input = Utils.loadInput("day03.in")
import cats.parse.{ Parser as P, Numbers }

val digitParser = Numbers.digit.rep(1, 3).map(_.toList.mkString.toInt)
val mulParser = ((P.string("mul(") *> digitParser <* P.char(',')) ~ (digitParser <* P.char(')'))).map(_ * _)
val inputParser = mulParser.backtrack.orElse(P.anyChar.as(0))

def part1: Int =
  inputParser.rep.parseAll(input).map(_.toList.sum) match
    case Left(value) => 0
    case Right(value) => value
    
val doParser = P.string("do()")

val dontParser = P.string("don't()")

val doAndDontParser = (inputParser.repUntil(dontParser) <* dontParser.void <* P.anyChar.repUntil(doParser)).map(_.toList.sum)

def part2: Int | Unit = 
  doAndDontParser.rep.parseAll(input) match
    case Left(value) => ()
    case Right(value) => value.toList.sum
  

@main def main: Unit = {
  val ans1 = part1
  println(ans1)  
  val ans2 = part2
  println(ans2)
}
