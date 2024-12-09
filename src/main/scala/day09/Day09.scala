package day09

import utils.Utils.*

import zio.Console.*
import zio.{*, given}
import scala.annotation.tailrec
import Block.*

enum Block {
  case Occupied(id: Int, spaces: Int)
  case Free(spaces: Int)

  def isFree =
    this match
      case Occupied(id, spaces) => false
      case Free(spaces) => true

  def isOccupied =
    !isFree
}

def decode(input: String) =
  ZIO.foreach(input.strip.zipWithIndex) { (length, index) =>
    ZIO.succeed {
      val n = length.toString.toInt

      if index % 2 == 0
      then {
        val id = index / 2
        Occupied(id, n)
      }
      else Free(n)
    }
  }



// My working solution but much worse then the one below.

  // val free = blocks.count(isFree)
  //
  // val toFill = blocks.reverse.filter(!isFree(_)).slice(0, free)
  //
  // def compactReq(blocks: List[Block], toFill: List[Block], combined: List[Block]): List[Block] =
  //   blocks match
  //     case head :: tail => {
  //       if isFree(head) then {
  //         toFill match
  //           case fhead :: ftail => compactReq(tail, ftail, fhead :: combined)
  //           case Nil => combined
  //
  //
  //       } else compactReq(tail, toFill, head :: combined)
  //     }
  //     case Nil => combined
  // ZIO.succeed(compactReq(blocks.slice(0, blocks.length - free), toFill, List.empty).reverse)

// Source: https://github.com/AlexMckey/AoC2024_Scala/blob/master/src/year2024/day09.scala
def compactP1(blocks: List[Block]) =
  @tailrec
  def compactReq(blocks: List[Block], acc: List[Block] = List.empty): List[Block] =
    blocks match {
      case (occu @ Occupied(_, _)) +: tail       => compactReq(tail, occu +: acc)
      case (init @ Free(_) +: _) :+ Free(_) => compactReq(init, acc)
      case Free(s) +: middle :+ (f @ Occupied(_, l)) if s == l => compactReq(middle, f +: acc)
      case Free(s) +: middle :+ (f @ Occupied(_, l)) if s > l => compactReq(Free(s - l) +: middle, f +: acc)
      case Free(s) +: middle :+ Occupied(id, l) if s < l => compactReq(middle :+ Occupied(id, l - s), Occupied(id, s) +: acc)
      case Seq(Free(_)) | Seq() => acc
      case _ => acc
    }

  ZIO.succeed(compactReq(blocks).reverse)

// Source: https://github.com/AlexMckey/AoC2024_Scala/blob/master/src/year2024/day09.scala
def compactP2(blocks: List[Block]) =
  @tailrec
  def compactReq(blocks: List[Block], acc: List[Block] = List.empty): List[Block] =
    blocks match {
        case init :+ (f @ Free(_)) => compactReq(init, f +: acc)
        case init :+ (f @ Occupied(_, l)) =>
          init.span{
            case Free(s) => s < l
            case _ => true
          } match
            case (_, Nil) => compactReq(init, f +: acc)
            case (before, Free(s) +: after) if s == l => compactReq((before :+ f) ++ after, Free(l) :: acc)
            case (before, Free(s) +: after) => compactReq((before :+ f :+ Free(s - l)) ++ after, Free(l) :: acc)
            case _ => acc
        case Nil => acc    
        case _ => acc
    }
  ZIO.succeed(compactReq(blocks))
    
    


def calcChecksum(compacted: List[Block]) = 
  // ZIO.succeed(compacted.zipWithIndex.map { (block, index) =>
  //   block match
  //     case Block.OccupiedBlock(id, _) => BigInt(id * index)
  //     case Block.FreeBlock => BigInt(0)
  // }.sum)

  // Source: https://github.com/AlexMckey/AoC2024_Scala/blob/master/src/year2024/day09.scala
  ZIO.succeed(compacted.foldLeft(0L -> 0) {
    case ((acc, idx), Occupied(id, len)) => (acc + id * len * (2 * idx + (len - 1)).toLong / 2) -> (idx + len)
    //case ((acc, idx), File(id, len)) => (acc + id * (idx until idx + len).sum.toLong) -> (idx + len)
    case ((acc, idx), Free(len)) => acc -> (idx + len)
  }._1)




object App extends ZIOAppDefault {
  def run = for {
    input     <- loadResource("day09.in")
    decoded   <- decode(input)
    compacted <- compactP1(decoded.toList)
    checksum  <- calcChecksum(compacted)
    _         <- printLine(checksum)
    compactedP2 <- compactP2(decoded.toList)
    checksumP2 <- calcChecksum(compactedP2)
    _       <- printLine(checksumP2)

  } yield () 
}
