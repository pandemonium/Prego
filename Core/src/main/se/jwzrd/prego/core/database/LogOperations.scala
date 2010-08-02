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

  def findLogById(id: Long): Option[Log] = transaction {
    logs lookup id
  }

  def createLog(account: Account): Log = transaction {
    logs insertOrUpdate Log(account)
  }

  def findLastLog(account: Account): Option[Log] = transaction {
    account.logs lastOption
  }

  def findOrCreateLog(account: Account): Log = transaction {
    findLastLog (account) getOrElse createLog (account)
  }
}