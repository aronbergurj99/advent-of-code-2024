package day07

import utils.Utils.*

import zio.Console._
import zio._
import scala.annotation.tailrec

type PuzzleInput = List[(BigInt, List[BigInt])]

enum Operation {
  case Addition(left: BigInt, right: BigInt)
  case Multiplication(left: BigInt, right: BigInt)
  case Concatenation(left: BigInt, right: BigInt)
}

object Operation {
  def apply(left: BigInt, right: BigInt, operator: Char) =
    operator match
      case '*' => Addition(left, right)
      case '+' => Multiplication(left, right)
      case '|' => Concatenation(left, right)
}

def parse(input: String) =
  ZIO.succeed(input.linesIterator.map { line =>
    val lineSplit = line.split(":")

    val sum = BigInt(lineSplit.head)
    val numbers = lineSplit.last.strip.split(" ").map(BigInt(_)).toList
    (sum, numbers)
  }.toList)


def genOperators(n: Int, ops: List[Char]) = 
  ZIO.iterate((List(List.empty[Char]), 0))(_._2 < n) { (combinations, i) =>
    ZIO.succeed {
      for {
        combination <- combinations
        op          <- ops
      } yield (op :: combination)
    }.map((_, i + 1))
  }.map(_._1)


def calibrate(testLine: (BigInt, List[BigInt]), operators: List[Char]) =
  val numbers = testLine.last
  val sum     = testLine.head  
  for {
    opers <- genOperators(testLine.last.length - 1, operators.toList)
  } yield (opers.map { ops =>
      ops.zip(numbers.tail).foldLeft(numbers.head) { (acc, curr) => 
        Operation(acc, curr.last, curr.head) match
          case Operation.Addition(left, right)       => left + right
          case Operation.Multiplication(left, right) => left * right
          case Operation.Concatenation(left, right)  => BigInt(left.toString + right.toString) 
      }
  }).filter(_ == sum).distinct.sum


def part1(input: PuzzleInput) = 
  ZIO.foreachPar(input) { testLine =>
    calibrate(testLine, List('+', '*'))
  }.map(_.sum)

def part2(input: PuzzleInput) = 
  ZIO.foreachPar(input) { testLine =>
    calibrate(testLine, List('+', '*', '|'))
  }.map(_.sum)

object App extends ZIOAppDefault {
  def run = for {
    input  <- loadResource("day07.in")
    parsed <- parse(input)
    ans1   <- part1(parsed)
    _      <- printLine(ans1)
    ans2   <- part2(parsed)
    _      <- printLine(ans2)
  } yield ()
}
