package day08

import zio.{*, given}
import zio.Console.*
import utils.Utils.*
import scala.math.*

type Pos = (Int, Int)
type Grid = Vector[Vector[Char]]

def parse(input: String) =
  ZIO.succeed {
    input.linesIterator.toVector.map(_.toVector)
  }

def makeAntinodes(grid: Grid) =
  ZIO.succeed {
    grid.zipWithIndex.foldLeft(Map.empty[Char, List[Pos]]) { (acc, indexedRow) =>
      indexedRow.head.zipWithIndex.filter(_.head != '.').foldLeft(acc) { (innerAcc, indexedCol) => 
        val antenna = indexedCol.head

        innerAcc.updated(
          antenna, 
          (indexedRow.last, indexedCol.last) :: innerAcc.getOrElse(antenna, List.empty))
      }
    }.values

      .flatMap(
      positions => positions
        .combinations(2)
        .map(v => (v.head, v.last))).flatMap(
          comb => List(findAntinodes(comb.head, comb.last), findAntinodes(comb.last, comb.head))).toList.distinct
  }

def findAntinodes(p1: Pos, p2: Pos) =
  val dx = p2._2 - p1._2
  val dy = p1._1 - p2._1

  (p1._1 - dy * 2, p1._2 + dx * 2)


def part1(grid: Grid, antinodes: List[Pos]) =
  ZIO.foreach(antinodes) { antinode => 
    ZIO.attempt {
      grid(antinode.head)(antinode.last)
      true
    }.orElseSucceed(false)
  }.map(_.count(identity))


object App extends ZIOAppDefault {
  def run = for {
    input  <- loadResource("day08.in")
    grid <- parse(input)
    antinodes   <- makeAntinodes(grid)
    ans1   <- part1(grid, antinodes)
    _      <- printLine(ans1)

  } yield ()
}
