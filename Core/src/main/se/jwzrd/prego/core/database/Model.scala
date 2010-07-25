package se.jwzrd.prego.core.database

import java.util.Date
import org.squeryl._

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Model extends Schema with DatabaseInfrastructure {
  import annotations._

  val logs = table[Log]("log")

  class Log(var name: String,
            @Column("created_time")
            var createdTime: Date) extends Entity

  object Log {
    def apply(): Log = apply("<new>")
    def apply(name: String): Log = apply(name, new Date())
    def apply(name: String, date: Date): Log = new Log(name, date)
  }

  val accounts = table[Account]("account")

  class Account(var username: String,
                var password: String,
                @Column("created_time")  
                var createdTime: Date) extends Entity

  object Account {
    def apply(name: String, password: String): Account =
      new Account(name, password, new Date())
  }

  /**
   * List isn't really a good way to expression a multitude of something
   * since it's concrete and generally not compatible with much anything.
   */

  // This model refers to everything else so loading a parent will spring-load
  // everything else. Not ideal! Relations should not be on the objects - that's
  // something for the DataServices to deal with.
/*
  case class User(name: String, log: Option[Log])

  case class Log(cycles: List[Cycle], exercises: List[Exercise])

  case class Cycle(sessions: List[SessionReport])

  case class Area(name: String)

  case class Exercise(name: String, area: Area)

  case class SessionReport(date: Date, slots: List[SlotReport])

  case class SlotReport(exercise: Exercise, sets: List[SetReport])

  case class SetReport(load: Double,
                       reps: Int,
                       sets: Int,
                       rpe: Double,
                       intensity: Double)
*/

  trait RtsCalculations {
    def rpe(reps: Int, intensity: Double): Double
    def intensity(reps: Int, rpe: Double): Double
    def reps(intensity: Double, rpe: Double): Int
  }

  trait DataServices {

  }
}