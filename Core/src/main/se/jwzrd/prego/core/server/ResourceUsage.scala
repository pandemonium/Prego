package se.jwzrd.prego.core.server

import java.io.OutputStream

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
/**
 * This thing is not good.
 */
trait ResourceUsage {
  type Streamed = {
    def getOutputStream(): OutputStream
  }

  def withResource(resource: Streamed)(using: OutputStream => Unit): Unit = {
    val os = resource getOutputStream

    try using(os)
    finally {
      os flush; os close
    }
  }
}