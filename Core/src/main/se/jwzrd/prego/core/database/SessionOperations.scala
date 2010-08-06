package se.jwzrd.prego.core.database

import org.squeryl.PrimitiveTypeMode._
import se.jwzrd.prego.core.util._

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait SessionOperations {
  import Model._

  def findSessionById(id: Long) = transaction {
    sessions lookup id
  }

  def findLastSessionByLog(log: Log): Option[Session] = transaction {
    log.sessions.lastOption
  }
}