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

def findAntennas(grid: Grid) =
  ZIO.succeed {
    grid.zipWithIndex.foldLeft(Map.empty[Char, List[Pos]]) { (acc, indexedRow) =>
      indexedRow.head.zipWithIndex.filter(_.head != '.').foldLeft(acc) { (innerAcc, indexedCol) => 
        val antenna = indexedCol.head

        innerAcc.updated(
          antenna, 
          (indexedRow.last, indexedCol.last) :: innerAcc.getOrElse(antenna, List.empty))
      }
    }.values.flatMap { positions =>
        positions.flatMap(pos => positions.filter(_ != pos).map((pos, _)))
    }
  }

def findAntinodes(p1: Pos, p2: Pos) =
  val dx = p2._2 - p1._2
  val dy = p1._1 - p2._1

  (p1._1 - dy * 2, p1._2 + dx * 2)


def part1(grid: Grid) =
  for {
    antennas  <- findAntennas(grid)
    antiNodes <- ZIO.foreach(antennas.map(findAntinodes).toList.distinct) { antiNode =>
      ZIO.attempt {
        grid(antiNode.head)(antiNode.last)
        true
      }.orElseSucceed(false)
    }.map(_.count(identity))
  } yield (antiNodes)

object App extends ZIOAppDefault {
  def run = for {
    input  <- loadResource("day08.in")
    grid <- parse(input)
    ans1   <- part1(grid)
    _      <- printLine(ans1)

  } yield ()
}
