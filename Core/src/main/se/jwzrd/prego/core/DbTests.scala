package se.jwzrd.prego.core

import database.Model
import database.Model.Log
import org.squeryl.PrimitiveTypeMode._
import java.util.Date

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object DbTests {
  def main(args: Array[String]) {
    println(Model)

    val three = transaction {
      val l: Log = new Log ("Skluno", new Date ())
      val two = Model.logs.insert(l)
      println(two + "; id=" + two.id)
      println(l.id)
      two
/*
      val q = from (Model.logs) { l => where (l.name === "Bonk") select (l) }

      q foreach (println)
*/
    }

    println(three.id)
  }
}