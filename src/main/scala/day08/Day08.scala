package day08

import zio.{*, given}
import zio.Console.*
import utils.Utils.*
import scala.math.*
import scala.util.Try

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

def findAntinodes(p1: Pos, p2: Pos, n: Int) =
  val dx = p2._2 - p1._2
  val dy = p1._1 - p2._1

  (p1._1 - dy * n, p1._2 + dx * n)


def part1(grid: Grid) =
  for {
    antennas  <- findAntennas(grid)
    antiNodes <- ZIO.foreach(antennas.map(findAntinodes(_, _, 2)).toList.distinct) { antiNode =>
      ZIO.attempt {
        grid(antiNode.head)(antiNode.last)
        true
      }.orElseSucceed(false)
    }.map(_.count(identity))
  } yield (antiNodes)



def part2(grid: Grid) =
  for {
    antennas  <- findAntennas(grid)
    antiNodes <- ZIO.foreach(antennas) { antenna => 
      def loop(n: Int, acc: List[Pos]): List[Pos] =
        val node = findAntinodes(antenna.head, antenna.last, n)

        Try(grid(node.head)(node.last)).toOption match
          case None => acc
          case Some(value) => loop(n + 1, node :: acc)

      ZIO.succeed(loop(1, List.empty))
    }
  } yield (antiNodes.flatMap(identity).toList.distinct.length)

object App extends ZIOAppDefault {
  def run = for {
    input  <- loadResource("day08.in")
    grid <- parse(input)
    ans1   <- part1(grid)
    _      <- printLine(ans1)
    ans2   <- part2(grid)
    _      <- printLine(ans2)
  } yield ()
}
