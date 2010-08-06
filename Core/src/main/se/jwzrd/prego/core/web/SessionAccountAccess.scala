package se.jwzrd.prego.core.web

import se.jwzrd.prego.core.web
import se.jwzrd.prego.core.database.Model.Account
import se.jwzrd.prego.core.server.http.Application.IntrinsicValues

/**
 * Should this trait also mean that it cannot be used without a loggedIn account?
 * How could that be expressed in a type-safe manner?
 * 
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait SessionAccountAccess {
  def loggedInAccountOption(implicit iv: IntrinsicValues): Option[Account] =
    iv.session.get (AccountKey)

  def loggedInAccount(implicit iv: IntrinsicValues): Account =
    iv session AccountKey

  def loggedInAccount_=(account: Account)(implicit iv: IntrinsicValues) =
    iv session AccountKey = account
}