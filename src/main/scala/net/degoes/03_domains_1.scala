package net.degoes
import net.degoes.spreadsheet.Value.Dbl
import net.degoes.spreadsheet.Value.Str
import _root_.shapeless.newtype

/*
 * INTRODUCTION
 *
 * In Functional Design, a functional domain consists of three things:
 *
 * 1. A functional model, which is an immutable data type that models a
 *    solution to problems in the domain of interest.
 *
 * 2. Constructors that allow constructing solutions to simple problems.
 *
 * 3. Operators that solving more complex problems by transforming
 *    and combining solutions for subproblems.
 *
 * Functional domains allow modeling solutions to problems in a specific domain.
 * Done properly, a small set of primitives can be so powerful, they can be used
 * compositionally to describe all possible solutions in that domain.
 *
 * A functional domain can be regarded as a type of internal domain-specific
 * language (DSL), which is designed specifically for expressing compositional
 * solutions to some category of domain problems.
 *
 * ZIO is an example of a domain for input/output, whose effect type lets you
 * solve async/concurrent/resourceful problems, and whose operators let you
 * assemble large solutions from small solutions.
 *
 * In this section, you'll learn about designing domains using ADTS,
 * constructors, and composable operators.
 */

/**
 * SPREADSHEET - EXERCISE SET 1
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet {
  trait Spreadsheet {
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  sealed trait Value
  object Value {
    final case class Error(message: String) extends Value
    final case class Str(value: String)     extends Value
    final case class Dbl(value: Double)     extends Value
  }

  /**
   * EXERCISE 1
   *
   * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
   * computed from a `Spreadsheet`.
   */
  final case class CalculatedValue(evaluate: Spreadsheet => Value) { self =>

    /**
     * EXERCISE 2
     *
     * Add an operator that returns a new `CalculatedValue` that is the negated version of this one.
     */
    def unary_- : CalculatedValue =
      CalculatedValue { spreadsheet =>
        self.evaluate(spreadsheet) match {
          case Dbl(value) => Dbl(-value)
          case Str(_)     => Error("Can only negate numeric values")
          case error      => error
        }
      }

    /**
     * EXERCISE 3
     *
     * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
     * calculated values.
     */
    def +(that: CalculatedValue): CalculatedValue =
      self.binaryOp(that)("Expected both values to be numeric") {
        case (Dbl(l), Dbl(r)) => Dbl(l + r)
      }

    /**
     * EXERCISE 4
     *
     * Add a binary operator `-` that returns a new `CalculatedValue` that is the difere;nce of the
     * two calculated values.
     */
    def -(that: CalculatedValue): CalculatedValue = ???

    protected def binaryOp(that: CalculatedValue)(error: String)(
      f: PartialFunction[(Value, Value), Value]
    ): CalculatedValue =
      f.lift((self.evaluate(spreadsheet), that.evaluate(spreadsheet))) match {
        case None          => Value.Error(error)
        case Option(value) => value
      }
  }
  object CalculatedValue {

    /**
     * EXERCISE 5
     *
     * Add a constructor that makes an `CalculatedValue` from a `Value`.
     */
    def const(contents: Value): CalculatedValue = CalculatedValue { _ =>
      contents
    }

    /**
     * EXERCISE 6
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue =
      CalculatedValue(s => s.valueAt(col, row)).evaluate(s)
  }

  /**
   * EXERCISE 7
   *
   * Describe a cell whose contents are the sum of the cells at (0, 0) and (1, 0).
   */
  lazy val cell1: Cell =
    Cell(42, 42, CalculatedValue.at(0, 0) + CalculatedValue(1, 0))
}

/**
 * ETL - EXERCISE SET 2
 *
 * Consider an application designed to extract, transform, and load data.
 */
object etl {
  import scala.util._

  /**
   * Represents a row of data.
   */
  final case class DataRow(row: Map[String, DataValue]) { self =>
    def delete(name: String): DataRow = DataRow(row - name)

    def map(name: String)(f: PartialFunction[DataValue, DataValue]): DataRow =
      row.get(name).fold(self)(v => f.lift(v).fold(self)(v => DataRow(row.updated(name, v))))

    def rename(oldName: String, newName: String): DataRow =
      DataRow(row.get(oldName).fold(row)(value => (row - oldName).updated(newName, value)))

    def coerce(name: String, dtype: DataType): DataRow =
      row.get(name).fold(self)(v => v.coerce(dtype).fold(self)(v => DataRow(row + (name -> v))))
  }

  /**
   * Represents a stream of data.
   */
  final case class DataStream(foreach: (Try[DataRow] => Unit) => Unit) { self =>
    def coerce(name: String, dtype: DataType): DataStream = self.map(_.coerce(name, dtype))

    def delete(name: String): DataStream = self.map(_.delete(name))

    def orElse(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach {
          case Failure(exception) => that.foreach(callback)
          case x                  => callback(x)
        }
      }

    def map(f: DataRow => DataRow): DataStream =
      DataStream(callback => self.foreach(a => callback(a.map(f))))

    def mapColumn(name: String)(f: PartialFunction[DataValue, DataValue]): DataStream =
      self.map(_.map(name)(f))

    def merge(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach(callback)
        that.foreach(callback)
      }

    def rename(oldName: String, newName: String): DataStream =
      self.map(_.rename(oldName, newName))
  }

  /**
   * EXERCISE 1
   *
   * Design a data type that models sources and sinks in an ETL pipeline. Assume
   * your business requires you to extract data from (and load data to) FTP sites,
   * URLs, AWS S3 buckets, and databases described by JDBC connection strings.
   *
   * Also mock out, but do not implement, a method on each repository type called
   * `load`, which returns a `DataStream`.
   */
  sealed trait DataRepo
  object DataRepo {
    final case class FTP(server: String, port: Int, username: String, password: String) extends DataRepo
    final case class JDBC(connection: URI)                                              extends DataRepo
    final case class S3(bucker: String, fileFormat: FileFormat)                         extends DataRepo
    final case class URL(url: java.net.URL, fileFormat: FileFormat)                     extends DataRepo
  }

  sealed trait FileFormat
  object FileFormat {
    case object Json extends FileFormat
    case object Csv  extends FileFormat
    case object Xml  extends FileFormat
  }

  /**
   * EXERCISE 2
   *
   * Design a data type that models the type of primitives the ETL pipeline
   * has access to. This will include string, numeric, and date/time data.
   */
  sealed trait DataType
  object DataType {
    case object String  extends DataType
    case object Double  extends DataType
    case object Instant extends DataType
  }

  /**
   * EXERCISE 3
   *
   * Design a data type that models a value. Every value should have a `DataType`
   * that identifies its type (string, numeric, or data/time), and a `coerce` method
   * to coerce the value into another type.
   *
   * Be sure to model null, string, and integer, at the very least!
   */
  sealed trait DataValue {
    def dataType: DataType

    def coerce(otherType: DataType): Option[DataValue]
  }
  object DataValue {
    final case class String(value: java.lang.String) extends DataValue {
      def dataType: DataType

      def coerce(otherType: DataType): Option[DataValue]
    }

    final case class Double(value: scala.Double) extends DataValue {
      def dataType: DataType

      def coerce(otherType: DataType): Option[DataValue]
    }

    final case class Instant(value: java.time.Instant) extends DataValue {
      def dataType: DataType

      def coerce(otherType: DataType): Option[DataValue]
    }

  }

  /**
   * EXERCISE 4
   *
   * `Pipeline` is a data type that models a transformation from an input data
   * set into an output data step, as a series of one or more individual
   * operations.
   *
   * Create a model of a pipeline, using `DataStream`.
   */
  final case class Pipeline(extractTransform: () => DataStream) { self =>

    /**
     * EXERCISE 5
     *
     * Add a `merge` operator that models the merge of the output of this
     * pipeline with the output of the specified pipeline.
     *
     * {{{
     * Merge Associativity:  (p1 merge p2) merge p3 == p1 merge (p2 merge p3)
     * Merge Identity:       p merge Pipeline.empty == Pipeline.empty merge p == p
     * Merge Commutativity:  p1 merge p2 == p2 merge p1
     * Merge Duplication:    ???
     * }}}
     */
    def merge(that: Pipeline): Pipeline =
      Pipeline { () =>
        val left  = self.extractTransform()
        val right = that.extractTransform()

        left.merge(right)
      }

    /**
     * EXERCISE 6
     *
     * Add an `orElse` operator that models applying this pipeline, but if it
     * fails, switching over and trying another pipeline.
     */
    def orElse(that: Pipeline): Pipeline =
      Pipeline { () =>
        val left  = self.extractTransform()
        val right = that.extractTransform()

        left.orElse(right)
      }

    /**
     * EXERCISE 7
     *
     * Add an operator to rename a column in a pipeline.
     */
    def rename(oldName: String, newName: String): Pipeline =
      Pipeline { () =>
        self.extractTransform().rename(oldName, newtype)
      }

    /**
     * EXERCISE 8
     *
     * Add an operator to coerce a column into a specific type in a pipeline.
     */
    def coerce(column: String, newType: DataType): Pipeline = ???

    /**
     * EXERCISE 9
     *
     * Add an operator to delete a column in a pipeline.
     */
    def delete(column: String): Pipeline = ???

    /**
     * EXERCISE 10
     *
     * To replace nulls in the specified column with a specified value.
     */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = ???
  }
  object Pipeline {

    /**
     * EXERCISE 11
     *
     * Add a constructor for `Pipeline` that models extraction of data from
     * the specified data repository.
     */
    def extract(repo: DataRepo): Pipeline =
      Pipeline(() => repo.load)
  }

  /**
   * EXERCISE 12
   *
   * Create a pipeline that models extracts data from a URL, replacing all null
   * "age" columns with "0" as the default age, which renames a column "fname"
   * into a column "first_name", and which coerces the "age" column into an
   * integer type.
   */
  lazy val pipeline: Pipeline =
    Pipeline.extract(
      DataRepo.URL(new java.net.URL("https://google.com"), FileFormat.Xml),
      replaceNull
    )
}

/**
 * REAL ESTATE APP - GRADUATION PROJECT
 *
 * Consider a real estate app that must regularly fetch third-party pricing data
 * according to specified schedules. These schedules can be quite complicated,
 * although they possess regular structure (e.g. every fifth Tuesday, and hourly
 * on Wednesdays). The business considers it acceptable to create the schedules in
 * code (rather than reading them from a database).
 */
object pricing_fetcher {
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Sunday    extends DayOfWeek
    case object Monday    extends DayOfWeek
    case object Tuesday   extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday  extends DayOfWeek
    case object Friday    extends DayOfWeek
    case object Saturday  extends DayOfWeek
  }

  final case class Time(minuteOfHour: Int, hourOfDay: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, monthOfYear: Int)

  /**
   * EXERCISE 1
   *
   * `Schedule` is a data type that models a schedule, which has the ability to
   * indicate whether at any given `Time`, it is time to fetch the pricing data set.
   */
  final case class Schedule(isFetchTime: Time => Boolean) { self =>
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     */
    def union(that: Schedule): Schedule =
      Schedule(now => self.isFetchTime(now) || that.isFetchTime(now))

    /**
     * EXERCISE 3
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the intersection of those schedules. That is, the fetch will occur
     * only when both of the schedules would have performed a fetch.
     */
    def intersection(that: Schedule): Schedule =
      Schedule(now => self.isFetchTime(now) && that.isFetchTime(now))

    /**
     * EXERCISE 4
     *
     * Create a unary operator that returns a schedule that will never fetch
     * when the original schedule would fetch, and will always fetch when the
     * original schedule would not fetch.
     */
    def negate: Schedule = Schedule(now => !self.isFetchTime(now))
  }
  object Schedule {

    /**
     * EXERCISE 5
     *
     * Create a constructor for Schedule that models fetching on specific weeks
     * of the month.
     */
    def weeks(weeks: Int*): Schedule =
      Schedule(now => weeks.contains(now.weekOfMonth))

    /**
     * EXERCISE 6
     *
     * Create a constructor for Schedule that models fetching on specific days
     * of the week.
     */
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule =
      Schedule(now => daysOfTheWeek.contains(now.dayOfWeek))

    /**
     * EXERCISE 7
     *
     * Create a constructor for Schedule that models fetching on specific
     * hours of the day.
     */
    def hoursOfTheDay(hours: Int*): Schedule =
      Schedule(now => hours.contains(now.hourOfDay))

    /**
     * EXERCISE 8
     *
     * Create a constructor for Schedule that models fetching on specific minutes
     * of the hour.
     */
    def minutesOfTheHour(minutes: Int*): Schedule =
      Schedule(now => minutes.contains(now.minuteOfHour))
  }

  /**
   * EXERCISE 9
   *
   * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 noon,
   * and at 5:30, 6:30, and 7:30 every Thursday.
   */
  lazy val schedule: Schedule = wednesdays.union(thursdays)

  lazy val sixAndNoon: Schedule = Schedule.hoursOfTheDay(6, 12)
  lazy val wednesdays: Schedule = Schedule.daysOfTheWeek(DayOfWeek.Wednesday).intersection(sixAndNoon)

  lazy val fiveThirty             = Schedule.hoursOfTheDay(5) intersection Schedule.minutesOfTheHour(30)
  lazy val sixThirty              = Schedule.hoursOfTheDay(6) intersection Schedule.minutesOfTheHour(30)
  lazy val sevenThirty            = Schedule.hoursOfTheDay(7) intersection Schedule.minutesOfTheHour(30)
  lazy val fiveOrSixOrSevenThirty = fiveThirty.union(sixThirty).union(sevenThirty)
  lazy val thursdays: Schedule    = Schedule.daysOfTheWeek(DayOfWeek.Thursday).intersection(fiveOrSixOrSevenThirty)
}
