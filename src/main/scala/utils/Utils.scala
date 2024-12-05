package utils

import scala.io.{Source, BufferedSource}
import zio._

object Utils {
  def loadInput(file: String) = {
    val source = Source.fromResource(file)

    try source.mkString finally source.close()
  }

  def loadResource(fileName: String) = {
    def open = ZIO.attempt(Source.fromResource(fileName))
    def close(source: BufferedSource) = ZIO.succeed(source.close())
    
    ZIO.acquireReleaseWith(open)(close){ source => 
      ZIO.attempt(source.mkString)
    }
  }
} 
