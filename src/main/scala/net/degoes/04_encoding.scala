package net.degoes
import net.degoes.email_filter2.EmailFilter.SubjectContains
import net.degoes.email_filter2.EmailFilter.BodyContains
import net.degoes.email_filter2.EmailFilter.SenderIn
import net.degoes.email_filter2.EmailFilter.RecipientIn

// Solution != Model of Solution

// FP = Total, Deterministic, Pure
// f(x)
//  - f is total (defined for all inputs)
//  - f is deterministic (when x == y, f(x) == f(y))
//  - f is pure (for computing the value of f(x) is the only effect of function application)

final case class Email()

/*
A Gmail-style EmailFilter:
  - test if an email matches the filter
  - persistence
 */
final case class EmailFilter1(accept: Email => Boolean, persist: () => String) // can store additional functions which would correspond to additional methods in the trait bellow

// why prefer final case class instead of trait:
// functions are values that have methods on them, e.g., andThen etc.
// methods inside traits are not values
// executable encoding!!!
// the accept function executes the filter

trait EmailFilter2 {
  def accept(email: Email): Boolean
  def persist(): String
}

final case class IStream1(createInputStream: () => java.io.InputStream)

trait IStream2 {
  def createInputStream(): java.io.InputStream
}

// ee - executable encoding
// de - declarative encoding
object ee_versus_de {
  import scala.collection.mutable.Queue

  type ProgramExecutor[+A] = Queue[Any] => A

  object ee {
    final case class Program[+A](run: ProgramExecutor[A]) { self =>
      def map[B](f: A => B): Program[B] = self.flatMap(a => Program(f(a)))

      // OPERATION 1
      def flatMap[B](f: A => Program[B]): Program[B] =
        Program(stack => f(self.run(stack)).run(stack))

      def push: Program[Unit] =
        for {
          a <- self
          _ <- Program.push(a)
        } yield ()
    }
    object Program {
      def apply[A](a: => A): Program[A] = unit.map(_ => a)

      // CONSTRUCTOR 1
      val unit: Program[Unit] = Program(_ => ())

      // CONSTRUCTOR 2
      def push[A](a: A): Program[Unit] =
        Program(stack => stack.enqueue(a))

      // CONSTRUCTOR 3
      def pop: Program[Any] = Program((stack: Queue[Any]) => stack.dequeue())

      def popInt: Program[Int] = pop.map(_.asInstanceOf[Int])

      def add: Program[Int] =
        for {
          v1 <- popInt
          v2 <- popInt
        } yield v1 + v2
    }

    val program =
      for {
        _ <- Program.push(1)
        _ <- Program.push(1)
        _ <- Program.add.push
        v <- Program.popInt
      } yield v

    program.run(Queue())
  }

  object de {
    sealed trait Program[+A] { self =>
      def map[B](f: A => B): Program[B] = self.flatMap(a => Program(f(a)))

      def flatMap[B](f: A => Program[B]): Program[B] =
        Program.FlatMap(self, f)

      def push: Program[Unit] =
        for {
          a <- self
          _ <- Program.push(a)
        } yield ()

      def run(stack: Queue[Any]): A = Program.compile(self)(stack)
    }
    object Program {
      private case object Unit                                                        extends Program[scala.Unit]
      private case object Pop                                                         extends Program[Any]
      private final case class Push[A](value: A)                                      extends Program[scala.Unit]
      private final case class FlatMap[A, B](program: Program[A], f: A => Program[B]) extends Program[B]

      def apply[A](a: => A): Program[A] = unit.map(_ => a)

      val unit: Program[Unit] = Unit

      def push[A](a: A): Program[Unit] = Push(a)

      def pop: Program[Any] = Pop

      def popInt: Program[Int] = pop.map(_.asInstanceOf[Int])

      def add: Program[Int] =
        for {
          v1 <- popInt
          v2 <- popInt
        } yield v1 + v2

      private def compile[A](self: Program[A]): ProgramExecutor[A] = { (stack: Queue[Any]) =>
        self match {
          case Program.Unit => ()

          case Push(value) =>
            stack.enqueue(value)

            ()
          case Pop => stack.dequeue()

          case FlatMap(program, f) =>
            val a = compile(program)(stack)

            compile(f(a))(stack)
        }
      }
    }

    val program =
      for {
        _ <- Program.push(1)
        _ <- Program.push(1)
        _ <- Program.add.push
        v <- Program.popInt
      } yield v

    program.run(Queue())
  }
}

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to encode functional domain
 * constructors and operators:
 *
 * 1. Using a function or interface, whose methods execute the solution. This is
 *    called the "executable" encoding in this course. It's a direct, executable
 *    encoding of a domain. If some functional domain is modeled with a class
 *    or case class, or an open trait that is implemented by classes, then it's
 *    probably an executable encoding.
 *
 * 2. Using a pure data structure, which declaratively describes the solution, but
 *    which does not perform the solution. It's an abstract, "declarative"
 *    encoding of a domain. If some functional domain type is modeled with a
 *    sealed trait, then it's probably an abstract encoding, where the subtypes
 *    of the sealed trait model individual operations and constructors in the
 *    domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which merely models a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the functional domain.
 *
 * Executable encodings are "open": anyone can add new constructors and
 * operators, without updating existing code. On the other hand, executable
 * encodings are not "introspectable": because they are not data, but rather,
 * opaque executable machinery, they cannot be serialized, optimized, or
 * converted to other encodings.
 *
 * Abstract encodings are "introspectable": because they are pure data, they
 * can be serialized, optimized, and converted to other encodings, assuming
 * their component parts have the same properties (not all abstract encodings
 * do; if you embed a function inside an abstract encoding, it's becomes
 * opaque). On the other hand, abstract encodings are "closed": no one can add
 * new constructors or operators, without updating existing code.
 *
 * Summarizing the difference between executable and abstract encodings:
 *
 *  - Executable encodings have open constructors/operators, but closed
 *    interpreters.
 *  - Declarative encodings have closed constructors/operators, but open
 *    interpreters.
 *
 * Note: Tagless-final an executable encoding, but where by making the "solutions"
 * polymorphic, the choice of executor can be deferred arbitrarily.
 *
 * Legacy code prefers executable encodings; while many benefits of Functional
 * Design can be seen best using abstract encodings.
 *
 */

/**
 * EDUCATION - EXERCISE SET 1
 *
 * Consider a console-based educational application that tests the user's
 * knowledge of key concepts.
 */
object education_executable {
  import education._

  sealed trait Quiz2 { self =>

    /**
     * EXERCISE 1
     *
     * Add an operator `+` that appends this quiz to the specified quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def +(that: Quiz2): Quiz2 = Quiz2.Then(self, that)

    /**
     * EXERCISE 2
     *
     * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model
     * this as pure data using a constructor for Quiz in the companion object.
     */
    def bonus: Quiz2 = Quiz2.Bonus(self)
  }
  object Quiz2 {
    final case class Then(first: Quiz2, second: Quiz2) extends Quiz2
    final case class Bonus(quiz: Quiz2)                extends Quiz2
    final case class Single(question: Question[_])     extends Quiz2

    def apply[A](question: Question[A]): Quiz2 = Single(question)
  }

  /**
   * EXERCISE 3
   *
   * Implement an interpreter for the `Quiz` model that translates it into
   * the interactive console operations that it describes, returning a
   * QuizResult value.
   */
  def run(quiz: Quiz2): QuizResult = {
    def compile(quiz: Quiz2): Quiz =
      quiz match {
        case Quiz2.Then(l, r) => compile(l) + compile(r)
        case Quiz2.Bonus(q) => compile(q).bonus
        case Quiz2.Single()
      }
  }

}

/**
 * DATA TRANSFORM - EXERCISE SET 2
 *
 * Consider an email marketing platform, which allows users to upload contacts.
 */
object contact_processing2 {
  import contact_processing._

  sealed trait SchemaMapping2 { self =>

    /**
     * EXERCISE 1
     *
     * Add a `+` operator that models combining two schema mappings into one,
     * applying the effects of both in sequential order.
     */
    def +(that: SchemaMapping2): SchemaMapping2 = SchemaMapping2.Then(self, that)

    /**
     * EXERCISE 2
     *
     * Add an `orElse` operator that models combining two schema mappings into
     * one, applying the effects of the first one, unless it fails, and in that
     * case, applying the effects of the second one.
     */
    def orElse(that: SchemaMapping2): SchemaMapping2 = SchemaMapping2.OrElse(self, that)
  }
  object SchemaMapping2 {
    final case class Then(first: SchemaMapping2, second: SchemaMapping2) extends SchemaMapping2
    final case class OrElse(first: SchemaMapping2, second: SchemaMapping2) extends SchemaMapping2
    final case class Rename(oldName: String, newName: String) extends SchemaMapping2
    final case class Delete(name: String) extends SchemaMapping2
    /**
     * EXERCISE 3
     *
     * Add a constructor for `SchemaMapping` models renaming the column name.
     */
    def rename(oldName: String, newName: String): SchemaMapping2 = Rename(oldName, newName)

    /**
     * EXERCISE 4
     *
     * Add a constructor for `SchemaMapping` that models deleting the column
     * of the specified name.
     */
    def delete(name: String): SchemaMapping2 = Delete(name)
  }

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `SchemaMapping` model that translates it into
   * into changes on the contact list.
   */
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] = ???

  /**
   * BONUS EXERCISE
   *
   * Implement an optimizer for the `SchemaMapping` model that pushes deletes to the front of the
   * schema mapping in cases where doing so wouldn't later the result.
   */
  def optimize(schemaMapping: SchemaMapping2): SchemaMapping2 =
    ???
}

/**
 * EMAIL CLIENT - EXERCISE SET 3
 *
 * Consider a web email interface, which allows users to filter emails and
 * direct them to specific folders based on custom criteria.
 */
object email_filter2 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  sealed trait EmailFilter { self =>

    /**
     * EXERCISE 1
     *
     * Add an "and" operator that models matching an email if both the first and
     * the second email filter match the email.
     */
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    /**
     * EXERCISE 2
     *
     * Add an "or" operator that models matching an email if either the first or
     * the second email filter match the email.
     */
    def ||(that: EmailFilter): EmailFilter = EmailFilter.Or(self, that)

    /**
     * EXERCISE 3
     *
     * Add a "negate" operator that models matching an email if this email filter
     * does NOT match an email.
     */
    def negate: EmailFilter = EmailFilter.Negate(self)
  }
  object EmailFilter {
    final case class And(l: EmailFilter, r: EmailFilter) extends EmailFilter
    final case class Or(l: EmailFilter, r: EmailFilter) extends EmailFilter
    final case class Negate(emailFilter: EmailFilter) extends EmailFilter
    final case class SubjectContains(string: String) extends EmailFilter
    final case class BodyContains(string: String) extends EmailFilter
    final case class SenderIn(senders: Set[Address]) extends EmailFilter
    final case class RecipientIn(recipients: Set[Address]) extends EmailFilter


    /**
     * EXERCISE 4
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * subject of an email contains the specified word.
     */
    def subjectContains(string: String): EmailFilter = SubjectContains(string)

    /**
     * EXERCISE 5
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * body of an email contains the specified word.
     */
    def bodyContains(string: String): EmailFilter = BodyContains(string)

    /**
     * EXERCISE 6
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * sender of an email is in the specified set of senders.
     */
    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    /**
     * EXERCISE 7
     *
     * Add a constructor for `EmailFilter` that models looking to see if the
     * recipient of an email is in the specified set of recipients.
     */
    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)
  }

  /**
   * EXERCISE 8
   *
   * Implement an interpreter for the `EmailFilter` model that translates it into
   * into tests on the specified email.
   */
  def matches(filter: EmailFilter, email: Email): Boolean =
    ???

  import EmailFilter._
  def executeToPredicate(filter: EmailFilter): Email => Boolean =
  filter match {
    case And(l, r)  =>
      val left = executeToPredicate(l)
      val right = executeToPredicate(r)
      (email: Email) => left(email ) && right(email)
    case Or(l, r) =>
      val left = executeToPredicate(l)
      val right = executeToPredicate(r)
      (email: Email) => left(email ) || right(email)
    case Negate(v) =>
      val value = !executeToPredicate(v)
      (email: Email) => value(email)
    case SubjectContains(string) => 
      (email: Email) => email.subject.contains(string)
    case BodyContains(string) => 
      (email: Email) => email.body.contains(string)
    case SenderIn(senders) => 
      (email: Email) => values.contains(email.sender)
    case RecipientIn(recipients) => 
      (email: Email) => email.to.exists(t => recipients.contains(t))
  }



  /**
   * EXERCISE 9
   *
   * Implement a function to make an English-readable description of an
   * `EmailFilter`.
   */
  def describe(filter: EmailFilter): Unit = ???
}

/**
 * SPREADSHEET - EXERCISE SET 4
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet2 {
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

  sealed trait CalculatedValue { self =>
    final case class Negate(self: CalculatedValue) extends CalculatedValue
    final case class Sum(self: CalculatedValue, that: CalculatedValue) extends CalculatedValue
    final case class Const(contents: Value) extends CalculatedValue
    final case class At(col: Int, row: Int) extends CalculatedValue

    /**
     * EXERCISE 1
     *
     * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
     * example, one operator could "negate" a double CalculatedValue.
     */
    def negate: CalculatedValue = ???

    /**
     * EXERCISE 2
     *
     * Add some operators to combine `CalculatedValue`. For example, one operator
     * could sum two double CalculatedValueessions.
     */
    def sum(that: CalculatedValue): CalculatedValue = ???
  }
  object CalculatedValue {

    /**
     * EXERCISE 3
     *
     * Add a constructor that makes an CalculatedValue from a Value.
     */
    def const(contents: Value): CalculatedValue = ???

    /**
     * EXERCISE 4
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue = ???
  }

  /**
   * EXERCISE 5
   *
   * Implement an interpreter for the `Value.CalculatedValue` model that translates it into
   * static cell contents by evaluating the CalculatedValueession.
   */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): Value = ???
}

/**
 * E-COMMERCE MARKETING - GRADUATION PROJECT
 *
 * Consider an e-commerce marketing platform where emails are sent to users
 * whose history matches specific patterns (for example, an event of adding
 * a product to a shopping card, followed by an abandonment of the web
 * session).
 */
object ecommerce_marketing {
  type Event = Map[Attribute, Value]

  sealed trait Attribute
  object Attribute {
    case object EventType      extends Attribute
    case object UserName       extends Attribute
    case object ShoppingCartId extends Attribute
    case object Email          extends Attribute
    case object WebSession     extends Attribute
    case object DateTime       extends Attribute
  }

  sealed trait Value
  object Value {
    final case class Str(value: String)                        extends Value
    final case class Id(value: String)                         extends Value
    final case class Email(value: String)                      extends Value
    final case class DateTime(value: java.time.OffsetDateTime) extends Value
  }

  object abstract_encoding {
    sealed trait Pattern { self =>
      def +(that: Pattern): Pattern = Pattern.Sequence(self, that)

      def atLeast(n: Int): Pattern = repeat(Some(n), None)

      def atMost(n: Int): Pattern = repeat(None, Some(n))

      def between(min: Int, max: Int): Pattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): Pattern = Pattern.Repeat(self, min, max)
    }
    object Pattern {
      case object HasAnyAttribute                                                   extends Pattern
      final case class HasAttribute(attr: Attribute)                                extends Pattern
      final case class HasValue(attr: Attribute, value: Value)                      extends Pattern
      final case class Sequence(first: Pattern, second: Pattern)                    extends Pattern
      final case class Repeat(pattern: Pattern, min: Option[Int], max: Option[Int]) extends Pattern

      val hasAnyAttribute: Pattern = HasAnyAttribute

      def hasAttribute(attr: Attribute): Pattern = HasAttribute(attr)

      def hasValue(attr: Attribute, value: Value): Pattern = HasValue(attr, value)
    }
    import Pattern._

    val example =
      hasAnyAttribute +
        hasAttribute(Attribute.ShoppingCartId)

    def matches(history: List[Event], pattern: Pattern): Boolean = {
      def loop(history: List[Event], pattern: Pattern): (List[Event], Boolean) =
        (pattern, history.headOption) match {
          case (HasAttribute(attr), Some(event))    => (history.tail, event.contains(attr))
          case (HasAnyAttribute, Some(event))       => (history.tail, true)
          case (HasValue(attr, value), Some(event)) => (history.tail, event.get(attr).map(_ == value).getOrElse(false))
          case (Sequence(first, second), _) =>
            val (leftHistory, leftMatch) = loop(history, first)

            if (leftMatch) loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _) =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)) {
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)
            }

            if (!baseline._2) baseline
            else {
              val after = (0 to (max - min)).foldLeft(baseline) {
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)
              }

              (after._1, true)
            }
          case _ => (history, false)
        }
      loop(history, pattern)._2
    }
  }

  /**
   * EXERCISE 1
   *
   * Develop an executable encoding of the pattern matcher. Instead of having
   * an ADT to represent a pattern, and then interpreting that on a user
   * history to see if there is a match, you will represent a pattern as a
   * function or an interface that is capable of testing the user history for
   * a match.
   */
  object executable_encoding {
    trait Pattern {
      def matches(history: List[Event], pattern: Pattern): Boolean

      def +(that: Pattern): Pattern = ???

      def atLeast(n: Int): Pattern = ???

      def atMost(n: Int): Pattern = ???

      def between(min: Int, max: Int): Pattern = ???

      def repeat(min: Option[Int], max: Option[Int]): Pattern = ???
    }
    object Pattern {
      val hasAnyAttribute: Pattern = ???

      def hasAttribute(attr: Attribute): Pattern = ???

      def hasValue(attr: Attribute, value: Value): Pattern = ???

      def partial(pf: PartialFunction[List[Event], (List[Event], Boolean)]): Pattern =
        Pattern{

        }
    }
  }
}
