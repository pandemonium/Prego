package se.jwzrd.prego.core.database

import java.util.Date
import org.squeryl._
import dsl.{ManyToOne, OneToMany}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Model extends Schema with DatabaseInfrastructure {
  import annotations._
  import PrimitiveTypeMode._

  // If I am to treat all entities as very lightweight so that all edits
  // take place in the database in real time. Then picking up "old" new
  // objects must work. Objects need to have some sort of "New / Dirty / Saved"
  // states. HTF that's going to work. Maybe just put that in Session
  // and pick it up from there. Will need some clean-up code too.

  val accounts = table[Account]("account")

  val logs = table[Log]("log")

  val sessions = table[Session]("session")

  val logToSessions = oneToManyRelation (logs, sessions) via ((l, s) => l.id === s.logId)

  val accountToLogs = oneToManyRelation (accounts, logs) via ((a, l) => a.id === l.accountId)

  class Log (@Column("account_id")
             var accountId: Long,
             var name: String,
             @Column("created_time")
             var createdTime: Date) extends Entity {
    lazy val sessions: OneToMany[Session] = logToSessions left this
    lazy val account: ManyToOne[Account] = accountToLogs right this
  }

  object Log {
    def apply(account: Account): Log = apply(account, account.username + "'s log")
    def apply(account: Account, name: String): Log = apply(account, name, new Date())
    def apply(account: Account, name: String, date: Date): Log = new Log(account id, name, date)
  }

  class Account (var username: String,
                 var password: String,
                 @Column("created_time")
                 var createdTime: Date) extends Entity {
    lazy val logs: OneToMany[Log] = accountToLogs left this
  }

  object Account {
    def apply(name: String, password: String): Account =
      new Account(name, password, new Date())
  }

  // Add unapply methods?
  object Session {
    def apply(log: Log, date: Date, microCycle: Int, numberInCycle: Int) =
      new Session (log id, date, microCycle, numberInCycle)
  }

  class Session (@Column("log_id")
                 var logId: Long,
                 var date: Date,
                 @Column("micro_cycle_no")
                 var microCycle: Int,
                 @Column("session_no")
                 var numberInCycle: Int) extends Entity {
    lazy val log: ManyToOne[Log] = logToSessions right this
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