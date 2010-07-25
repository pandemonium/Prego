package se.jwzrd.prego.core.database

import se.jwzrd.prego.core.database.Model._
import org.squeryl.PrimitiveTypeMode._

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait AccountOperations {
  def authenticate(username: String, password: String): Option[Account] = transaction {
    accounts where {a => a.username === username and a.password === password} headOption
  }
}