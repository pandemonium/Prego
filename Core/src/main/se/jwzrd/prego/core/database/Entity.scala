package se.jwzrd.prego.core.database

import org.squeryl.KeyedEntity

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Entity extends KeyedEntity[Long] {
  var id = 0L
}