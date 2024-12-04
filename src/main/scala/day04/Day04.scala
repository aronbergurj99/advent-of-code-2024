package day04

import zio.ZIOAppDefault
import zio.Console._
import zio._
import scala.io.Source
import scala.io.BufferedSource

import java.lang.IndexOutOfBoundsException

type Matrix = Seq[Seq[Char]]

def loadResource(fileName: String) = {
  def open = ZIO.attempt(Source.fromResource(fileName))
  def close(source: BufferedSource) = ZIO.succeed(source.close())
  
  ZIO.acquireReleaseWith(open)(close){ source => 
    ZIO.attempt(source.mkString)
  }
}

def parse(input: String): UIO[Matrix] =
  ZIO.succeed(input.linesIterator.map { line =>
    line.toCharArray.toSeq
  }.toSeq)


val directions: Seq[(Int, Int)] = Seq(
    (1, 0),   // Up
    (-1, 0),  // Down
    (0, -1),  // Left   
    (0, 1),   // Right
    (1, 1),   // UpRight
    (1, -1),  // UpLeft
    (-1, 1),  // DownRight
    (-1, -1), // DownLeft
  )


def searchXmasAtPos(matrix: Matrix, pos: (Int, Int)) =
  ZIO.foreach(directions){ (row, col) => 
    ZIO.foreach("XMAS".zipWithIndex) { (char, index) =>
      ZIO.attempt({
        val currRow = pos.head + (row * index)
        val currCol = pos.last + (col * index)

        char == matrix(currRow)(currCol)

      }).refineToOrDie[IndexOutOfBoundsException].orElse(ZIO.succeed(false))
    }.map(_.forall(_ == true))
  }.map(_.count(_ == true))

def searchXmasAll(matrix: Matrix) =
  ZIO.foreach(matrix.zipWithIndex) { (row, row_index) =>
    ZIO.foreach(row.zipWithIndex) { (col, col_index) =>
      searchXmasAtPos(matrix, (row_index, col_index))
    }.map(_.sum)
  }.map(_.sum)


def part1(matrix: Matrix) =
  searchXmasAll(matrix)

def crossMas(matrix: Matrix, aPos: (Int, Int)) =
  ZIO.attempt({
    // Top Left to bottom right
    val mainDiagonal = Seq(matrix(aPos.head + 1)(aPos.last - 1), matrix(aPos.head)(aPos.last), matrix(aPos.head - 1)(aPos.last + 1)).mkString
    // Top Right to bottom right
    val antiDiagonal = Seq(matrix(aPos.head + 1)(aPos.last + 1), matrix(aPos.head)(aPos.last), matrix(aPos.head - 1)(aPos.last - 1)).mkString

    Seq(mainDiagonal, mainDiagonal.reverse, antiDiagonal, antiDiagonal.reverse).map(_ == "MAS").count(identity) >= 2
  }).orElseSucceed(false)

def part2(matrix: Matrix) =
  ZIO.foreach(matrix.indices) { row => 
    ZIO.foreach(matrix(row).indices) { col =>
      val char = matrix(row)(col) 
      if char == 'A'
      then {
        crossMas(matrix, (row, col))
      } else ZIO.succeed(false)
    }.map(_.count(identity))
  }.map(_.sum)

object Day04 extends ZIOAppDefault {
  def run = program

  val program = 
    for {
      input  <- loadResource("day04.in")
      parsed <- parse(input)
      ans1   <- part1(parsed)
      _      <- printLine(s"Answer to part1 is: $ans1")
      ans2   <- part2(parsed)
      _      <- printLine(s"Answer to part2 is: $ans2")
    } yield ()  
}

