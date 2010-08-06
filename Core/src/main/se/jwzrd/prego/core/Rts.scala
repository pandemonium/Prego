package se.jwzrd.prego.core
package `import`

import io.Source
import server.Parsing
import java.lang.String
import collection.Iterator
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers
import java.text.SimpleDateFormat
import java.util.Date

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Rts {

  object Dsl extends JavaTokenParsers {
    case class SessionReport(week: Int, day: Int, date: Date, reports: List[SlotReport])
    case class Day(n: Int, date: Date)
    case class SlotReport(exercise: Exercise, setsAndReps: List[SlotReportEntry])
    case class Exercise(name: String, oneRm: Int)
    case class SlotReportEntry(weight: Double, reps: Int, sets: Int, rpe: Double, intensity: Int)

    lazy val sessionReport = week ~ day ~ (slotReport *) ^^
      { case week ~ day ~ slotReports => SessionReport (week, day.n, day.date, slotReports) }

    lazy val week = "Week" ~> integer

    lazy val day = "Day" ~> (integer <~ "- ") ~ date ^^ { case index ~ date => Day (index, date) }
    lazy val date = """[^\s]+""".r ^^ { new SimpleDateFormat("MM/dd/yy") parse(_) }

    lazy val slotReport = exercise ~ (slotReportEntry *) ^^
      { case exercise ~ entries => SlotReport (exercise, entries) }

    lazy val exercise = (legalExerciseName <~ "- 1RM:") ~ (integer <~ "kgs") ^^
      { case name ~ oneRm => Exercise (name, oneRm) }
    lazy val legalExerciseName = """[0-9A-Za-z\(\),\s]+""" r

    lazy val slotReportEntry = weight ~ reps ~ sets ~ rpe ~ intensity ^^
      { case weight ~ reps ~ sets ~ rpe ~ intensity => SlotReportEntry (weight, reps, sets, rpe, intensity) }
    
    lazy val weight = floatingPoint <~ "kgs x"
    lazy val reps = integer <~ "reps x"
    lazy val sets = integer <~ "sets @"
    lazy val rpe = floatingPoint <~ "RPE -"
    lazy val intensity = integer <~ "% of 1RM"

    lazy val integer = wholeNumber ^^ { _ toInt }
    lazy val floatingPoint = floatingPointNumber ^^ { _ toDouble }
  }

  def main(args: Array[String]) {
    val input =
      """
        Week 47
        Day 3 - 7/30/10
        Squat - 1RM: 250 kgs
        230 kgs x 1 reps x 1 sets @ 9 RPE - 92% of 1RM
        250 kgs x 1 reps x 1 sets @ 10 RPE - 100% of 1RM
        225 kgs x 1 reps x 1 sets @ 9 RPE - 90% of 1RM
        Benchpress, pause (4ct) - 1RM: 161 kgs
        130 kgs x 3 reps x 1 sets @ 7.5 RPE - 81% of 1RM
        145 kgs x 3 reps x 1 sets @ 8.5 RPE - 90% of 1RM
        152.5 kgs x 2 reps x 1 sets @ 10 RPE - 95% of 1RM
        140 kgs x 3 reps x 1 sets @ 8 RPE - 87% of 1RM
        145 kgs x 3 reps x 1 sets @ 9 RPE - 90% of 1RM
        Deadlift - 1RM: 320 kgs
        290 kgs x 1 reps x 1 sets @ 8 RPE - 91% of 1RM
        310 kgs x 1 reps x 1 sets @ 9.5 RPE - 97% of 1RM
        320 kgs x 1 reps x 1 sets @ 10 RPE - 100% of 1RM
      """

    val sr = Dsl.parseAll(Dsl.sessionReport, input)
    println(sr)

    val lines = Source fromString input getLines () map (_ trim) filterNot (_ isEmpty)

    lines foreach println
  }
}