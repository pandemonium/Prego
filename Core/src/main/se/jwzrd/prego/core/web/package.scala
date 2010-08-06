package se.jwzrd.prego.core

import database.Model.Account
import server.http._

package object web {
  object AccountKey extends Key {
    type Value = Account
  }
}