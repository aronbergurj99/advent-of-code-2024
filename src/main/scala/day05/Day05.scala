package day05

import zio._
import zio.Console._
import utils.Utils
import scala.annotation.tailrec

type Rules = Map[Int, List[Int]]
type Updates = List[Int]

def parseRules(input: String) =
  ZIO.succeed(input.linesIterator.foldLeft(Map.empty[Int, List[Int]]) { (acc, line) =>
    val splitResult = line.split("\\|")
    val key = splitResult.head.toInt
    val value = splitResult.last.toInt

    acc.updated(key, value :: acc.getOrElse(key, List.empty))
  })

def parseUpdates(input: String) = 
  ZIO.succeed(input.linesIterator.map { line =>
    line.split(",").map(_.toInt).toList
  }.toVector)

def parse(input: String) =
  val inputSplit = input.split("\n\n").toVector

  for {
    rules   <- parseRules(inputSplit.head)
    updates <- parseUpdates(inputSplit.last)
  } yield (rules, updates)

def verify(rules: Rules, updates: Updates) = 
  @tailrec
  def verifyRec(remaining: Updates, seen: Set[Int], valid: Boolean): Boolean =
    remaining match {
      case Nil => valid
      case head :: tail => verifyRec(tail, seen + head, valid && {
        rules.getOrElse(head, List.empty).forall(rule => !seen.contains(rule) )
      })
    }
  verifyRec(updates, Set.empty[Int], true)

def midpoint(updates: Updates) =
  updates(updates.length / 2)


def part1(rules: Rules, allUpdates: Vector[Updates]) =
  ZIO.succeed(allUpdates.filter(verify(rules, _)).map(midpoint).sum)


def part2(rules: Rules, allUpdates: Vector[Updates]) =
  ZIO.succeed(allUpdates.filter(!verify(rules, _)).map(_.sortWith((a, b) => {
    rules(b).contains(a)
  })).map(midpoint).sum)
  


object App extends ZIOAppDefault {
  def run = program

  def program = {
    for {
      input  <- Utils.loadResource("day05.in")
      parsed <- parse(input)
      ans1   <- part1(parsed._1, parsed._2)
      _      <- printLine(ans1)
      ans2   <- part2(parsed._1, parsed._2)
      _      <- printLine(ans2)
    } yield ()
  }
}
