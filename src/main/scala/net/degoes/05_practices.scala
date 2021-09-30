package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter =
      !(!self && !that) // follows some kind of law allowing OR to define like that

    def ^^(that: EmailFilter): EmailFilter =
      (self || that) && !(self && that) // exclusive OR

    def unary_! : EmailFilter = EmailFilter.Not(self)
  }
  object EmailFilter {
    final case object Always                                    extends EmailFilter
    final case class Not(value: EmailFilter)                    extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class SenderEquals(target: Address)              extends EmailFilter
    final case class RecipientEquals(target: Address)           extends EmailFilter
    final case class BodyContains(phrase: String)               extends EmailFilter
    final case class SubjectContains(phrase: String)            extends EmailFilter

    val acceptAll: EmailFilter = Always

    val rejectAll: EmailFilter = !acceptAll

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = SenderNotEquals(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter =
      sender.foldLeft(rejectAll) {
        case (acc, address) => acc || senderIs(address)
      }

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait Turtle { self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  object declarative {
    sealed trait TurtleDrawing { self =>
      def ++(that: TurtleDrawing): TurtleDrawing = TurtleDrawing.AndThen(self, that)

      def turnLeft(degrees: Int): TurtleDrawing = self ++ TurtleDrawing.TurnLeft(degrees)

      def turnRight(degrees: Int): TurtleDrawing = self ++ TurtleDrawing.TurnRight(degrees)

      def goForward(): TurtleDrawing = self ++ TurtleDrawing.GoForward()

      def goBackward(): TurtleDrawing = self ++ TurtleDrawing.GoBackward()

      def draw(): TurtleDrawing = self ++ TurtleDrawing.Draw()
    }
    object TurtleDrawing {
      final case class TurnLeft(degrees: Int)                               extends TurtleDrawing
      final case class TurnRight(degrees: Int)                              extends TurtleDrawing
      case object GoForward                                                 extends TurtleDrawing
      case object GoBackward                                                extends TurtleDrawing
      case object Draw                                                      extends TurtleDrawing
      final case class AndThen(first: TurtleDrawing, second: TurtleDrawing) extends TurtleDrawing

      def start: TurtleDrawing = turnLeft(0)

      def turnLeft(degrees: Int): TurtleDrawing = TurnLeft(degrees)

      def turnRight(degrees: Int): TurtleDrawing = TurnRight(360 - degrees)

      def goForward(): TurtleDrawing = GoForward()

      def goBackward(): TurtleDrawing = GoBackward()

      def draw(): TurtleDrawing = Draw
    }

    val drawing =
      start
  }

  object executable {
    final case class TurtleDrawing(unsafeDraw: Turtle => Unit) {
      def ++(that: TurtleDrawing): TurtleDrawing =
        TurtleDrawing { turtle =>
          self.draw(turtle)
          that.draw(turtle)
        }

      def turnLeft(degrees: Int): TurtleDrawing =
        TurtleDrawing { turtle =>
          _.turnLeft(degrees)
        }

      def turnRight(degrees: Int): TurtleDrawing =
        TurtleDrawing { turtle =>
          _.turnRight(degrees)
        }

      def goForward(): TurtleDrawing =
        TurtleDrawing { turtle =>
          _.goForward()
        }

      def goBackward(): TurtleDrawing =
        TurtleDrawing { turtle =>
          _.goBackward()
        }

      def draw(): TurtleDrawing =
        TurtleDrawing { turtle =>
          _.draw
        }
    }
    object TurtleDrawing {
      def start: TurtleDrawing = TurtleDrawing(_ => ())

      def turnLeft(degrees: Int): TurtleDrawing = start.turnLeft(degrees)

      def turnRight(degrees: Int): TurtleDrawing = start.turnRight(degrees)

      def goForward(): TurtleDrawing = start.goForward()

      def goBackward(): TurtleDrawing = start.goBackward()

      def draw(): TurtleDrawing = start.draw()
    }
  }
}
