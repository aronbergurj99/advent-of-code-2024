package day06

import zio.Console._
import zio._
import zio.prelude._
import utils.Utils
import scala.annotation.tailrec
import scala.util.Try

type Pos = (Int, Int)
val obstacle = '#'

case class AocMap(rows: Vector[Vector[Char]]) {
  def apply(pos: Pos) =
    rows(pos._1)(pos._2)

  def updated(pos: Pos, value: Char) =
    AocMap(rows.updated(pos._1, rows(pos._1).updated(pos._2, value)))

  override def toString(): String =
    rows.map(_.mkString).mkString("\n")

  def positions: Iterator[Pos] =
    for {
      row <- rows.indices.iterator
      col <- rows(row).indices.iterator
    } yield (row, col)
}

given Associative[Pos] with {
  def combine(l: => Pos, r: => Pos): Pos = {
    (l._1 - r._1, l._2 + r._2)
  }
}


def parse(input: String) =
  val map = AocMap(input.linesIterator.map { line =>
    line.toCharArray.toVector
  }.toVector)

  for {
    carrot <- findCarrot(map)
  } yield (map, carrot)

def findCarrot(map: AocMap) = {
  @tailrec
  def findCarrotReq(map: AocMap, curr: Int): Pos =
    val rowIndex = curr / map.rows.length
    val colIndex = curr % map.rows.length

    val col = map((rowIndex, colIndex))

    if col == '^'
    then (rowIndex, colIndex)
    else findCarrotReq(map, curr + 1)

  ZIO.succeed(findCarrotReq(map, 0))
}

def rotate(dir: Pos) =
  (-dir._2, dir._1)

def makePrediction(map: AocMap, guard: Pos) = 
  @tailrec
  def predictReq(curr: Pos, dir: Pos, pMap: AocMap, visited: Set[(Pos, Pos)]): (AocMap, Boolean) =
    val next = curr <> dir
    val column = Try[Char](pMap(next)).toOption

    column match
      case None => (pMap, false) // Out of bounds
      case Some(value) => {
        val blocked = value == '#' | value == 'O'
        if blocked & visited.contains((next, dir))
        then (pMap, true)
        else if blocked then predictReq(curr, rotate(dir), pMap, visited + ((next, dir)))
        else {
          predictReq(next, dir, pMap.updated(next, 'X'), visited)
        }
      }

  ZIO.succeed(predictReq(guard, (1,0), map, Set.empty))

def part1(input: AocMap, start: Pos) = {
  makePrediction(input, start).map(_._1.rows.map(_.filter(_ == 'X').length).sum)
  for {
    prediction <- makePrediction(input, start)
  } yield (prediction._1.rows.map(_.filter(_ == 'X').length).sum, prediction._1)
}

def part2(input: AocMap, start: Pos) = {
  ZIO.foreachPar(input.positions.filter(input(_) == 'X').toList) { pos => 
    val map = input.updated(pos, 'O')
    makePrediction(map, start).map(_._2)
  }.map(_.count(identity))}

object App extends ZIOAppDefault {
  def run = program

  val program = for {
    input         <- Utils.loadResource("day06.in")
    (map, carrot) <- parse(input)
    ans1          <- part1(map, carrot)
    _             <- printLine(ans1._1)
    ans2          <- part2(ans1._2, carrot)
    _             <- printLine(ans2)
  } yield ()
}
