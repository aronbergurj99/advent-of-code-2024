package utils

import scala.io.Source

object Utils {
  def loadInput(file: String) = {
    val source = Source.fromResource(file)

    try source.mkString finally source.close()
  }
} 
