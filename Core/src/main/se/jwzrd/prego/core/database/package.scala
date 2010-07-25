package se.jwzrd.prego.core

import database.Model.Account
import server.http._

package object database {
  object AccountKey extends Key {
    type Value = Account
  }
}