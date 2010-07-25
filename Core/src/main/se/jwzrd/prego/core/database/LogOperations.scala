package se.jwzrd.prego.core.database

import se.jwzrd.prego.core.database.Model._
import org.squeryl.PrimitiveTypeMode._

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait LogOperations {
  def findAllLogs = transaction {
    from (logs) { l => select (l) } toSeq
  }
}