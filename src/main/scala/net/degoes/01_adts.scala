package net.degoes

import java.time.Instant
import java.util.Date

/*

Algebraic Data Types (ADTs)

Any data type compose from:

  - Case classes (Products)
  - Sealed traits (Sums)
  - Primitives (Int, String, ...)

  Algebra of types (Products and Sums)
 */

// Best practice for code organization:
// put all constructor in the companion object to enhance discoverability!

// Product Type: A * B * C * D
//  Constructor: (A, B, C, D) => A * B * C * D
//  Projectors:
//    1. A * B * C * D => A
//    2. A * B * C * D => B
//    3. A * B * C * D => C
//    4. A * B * C * D => D
final case class User(email: String, hashedPassphrase: String, salt: Long, name: String)
object User {
  def fromJSON(jsonString: String): User = ???

  def user = User("sherlock", "fefefe", 123L, "Sherlock Holmes")

  user.email
  user.name
  user.salt
  user.hashedPassphrase

  user.copy(name = "Sherlock Holmes II")

  // User.apply
  // Filds
  // equals, hashCode
  // User.unapply

  user match {
    case User(email, _, _, _) => println(email)
  }
}

// Sum Type: A + B + C + D
sealed trait Ticket
object Ticket {
  // best practice is to put constructors for constructor in the companion object!
  final case class Online(url: String)      extends Ticket
  final case class Offline(address: String) extends Ticket
}

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  final case class CreditCard(number: Int, name: String, exp: Date, securityCode: Int)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product
  object Product {
    final case class Physical(name: String, price: Currency)            extends Product
    final case class Digital(name: String, price: Currency)             extends Product
    final case class Event(name: String, venue: Place, price: Currency) extends Product
  }

  final case class Currency()
  final case class Place()

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme
  object PricingScheme {
    final case class OneTime(price: Int)                                 extends PricingScheme
    final case class Recurring(price: Int, setupFee: Int, interval: Int) extends PricingScheme
  }
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  final case class Event(id: Int, time: Instant, payload: Payload)

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!

  sealed trait Payload
  object Payload {
    final case class User(userName: String, userDetails: UserEventDetails)    extends Payload
    final case class Device(deviceId: Int, deviceDetails: DeviceEventDetails) extends Payload

  }

  sealed trait UserEventDetails
  object UserEventDetails {
    final case class Purchase(item: String, price: Double) extends UserEventDetails
    case object UserAccountCreated                         extends UserEventDetails

  }

  sealed trait DeviceEventDetails
  object DeviceEventDetails {
    final case class SensorUpdated(reading: Option[Double]) extends DeviceEventDetails
    case object DeviceActivated                             extends DeviceEventDetails
  }

}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  type Document

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  type AccessType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  type DocPermissions
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  final case class Customer(name: String, accounts: Set[Account])

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  sealed trait AccountType
  object AccountType {
    case object Checking                                extends AccountType
    final case class Saving(interestRate: InterestRate) extends AccountType
  }

  type InterestRate

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  final case class Account(
    accountType: AccountType,
    owner: Customer,
    holdings: Map[Currency, BigDecimal],
    sharedWith: Map[Customer, Permissions]
  )

  sealed trait Currency
  final case class Permissions(read: Boolean, share: Boolean)
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
