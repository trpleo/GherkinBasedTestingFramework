package steps

import com.lightbend.lagom.scaladsl.api.ServiceCall
import cucumber.api.scala.{ EN, ScalaDsl }
import org.scalatest.Matchers._
import steps.WordTestingStepDefs.{ Limit, MockServiceCall, User, _ }
import ebdd.World

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{ Duration, _ }
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps
import scala.util.Random
import ebdd.CategoryConverters._

class WordTestingStepDefs extends ScalaDsl with EN {

  private val stateNamePattern = """(\w+\d*)"""
  private val stringValuePattern = """(.+)"""
  private val intValuePattern = """(\d+)"""

  val serviceCallRspId = "service-call-result"

  val mockServiceCallDelay = 500 millis
  implicit val timeout = mockServiceCallDelay.mul(2.0d)

  type ThisRQ = (String, String)
  type ThisRSP = User
  // hack: X[RQ, RSP] is World#HRecId[RQ, RST]. Created to avoid compilation error...
  type X[RQ, RSP] = PartialFunction[World#RequestResponse[RQ, RSP], Future[String]]

  var world: World = _

  Before { scenario =>
    world = new World()
  }

  After { scenario =>
    world.clear()
  }

  Given(s"""^User $stateNamePattern exists in the system with name "$stringValuePattern" - GENERAL$$""") { (userId: String, uName: String) =>
    val uid1st = "const-id"

    // create a "service call" which actually will be tested.
    val mockCall = MockServiceCall[ThisRQ, ThisRSP]({ case (id, n) => User(id, n, Random.nextInt()) }).doServiceCall(mockServiceCallDelay)

    // call service, and update world with the given service call's response. Identifier is a constrant string.
    world.mutateWorld[ThisRQ, ThisRSP](mockCall.invoke, (userId, uName), uid1st)

    // this function's return value will be used as the identifier of the record stored in the history.
    // when search for the given history record, a string will be defined. If the given string and the
    // return value of the actual history record would be equal, the record will be returned. The result
    // will return after the Future (inherited from the `World.this.FUTUREITHER[RSP]`) will be finished.
    val fId: X[ThisRQ, ThisRSP] = {
      case in => in.response.value.map {
        case Left(t) => s"ERROR: [${t.getMessage}]"
        case Right(u) => u.id
      }
    }

    // call service, and update world with the given service call's response. Identifier is a result of a computation.
    val userId2 = s"$userId-II"
    val f: ThisRQ => Future[ThisRSP] = mockCall.invoke
    world.updateWorld[ThisRQ, ThisRSP](f, (userId2, uName), fId)

    // check the given objects, based on their identifier
    world.checkResult[ThisRSP]({
      reply =>
        reply shouldBe a[ThisRSP] // note: just to be sure, however, the response must be properly typed
        reply.id should ===(userId)
        reply.name should ===(uName)
    }, uid1st)

    world.checkResult[ThisRSP]({
      reply =>
        reply shouldBe a[ThisRSP] // note: just to be sure, however, the response must be properly typed
        reply.id should ===(userId2)
        reply.name should ===(uName)
    }, userId2)
  }

  Given(s"""^User $stateNamePattern exists in the system with name "$stringValuePattern"$$""") { (userId: String, uName: String) =>
    // create a "service call" which actually will be tested.
    val mockCall = MockServiceCall[ThisRQ, ThisRSP]({ case (id, n) => User(id, n, Random.nextInt()) }).doServiceCall(mockServiceCallDelay)

    // call service, and update world with the given service call's response. Identifier is a constrant string.
    world.mutateWorld[ThisRQ, ThisRSP](mockCall.invoke, (userId, uName), userId)
  }

  Given(s"""^the cumulative $stateNamePattern limit rule $stateNamePattern exists in DC limit management with amount $stateNamePattern with currency $stateNamePattern$$""") {
    (bankName: String, limitId: String, amountId: String, currencyId: String) =>
      type Currency = String
      type Amount = Int
      val amnt = 30 // rnd(0, 100)

      val mockCreateLimitRule = MockServiceCall[(Amount, Currency, String), Limit]({ case (a, c, bn) => Limit(a, c, bn) }).doServiceCall(mockServiceCallDelay)

      world.mutateWorld[(Amount, Currency, String), Limit](mockCreateLimitRule.invoke, (amnt, currencyId, bankName), limitId)
      world.deriveWorld[Limit, Amount](limitId, amountId, { limit: Limit => limit.amount })
      world.deriveWorld[Limit, Currency](limitId, currencyId, { limit: Limit => limit.currency })
  }

  Given(s"""^User $stateNamePattern has Account $stateNamePattern is given with id number "$stringValuePattern" with balance $intValuePattern in currency $stringValuePattern$$""") {
    (uid: String, accId: String, accountNo: String, balance: Int, currency: String) =>

      type AccountNumber = String
      type Balance = Int
      type Currency = String
      type AccTuple = (AccountNumber, Balance, Currency, User)

      val mockCreateAccount = MockServiceCall[AccTuple, Account]({ case (a, b, c, o) => Account(a, b, c, o) }).doServiceCall(mockServiceCallDelay)
      val owner =
        Await.result(
          //          world.getRecordById[User](uid).response.value.flatMap {
          //            case Right(u) => Future.successful(u)
          //            case Left(t) => Future.failed(t)
          //          },
          world.getRecordByIdSync[User](uid).response.toFuture(),
          timeout.+(1 second) // a bit more than the timeout for getHistoryRecordById[T]
        )

      val rq: AccTuple = (accountNo, balance, currency, owner)
      world.mutateWorld[AccTuple, Account](mockCreateAccount.invoke, rq, accId)
      world.deriveWorld[Account, Account](accId, accountNo, { a: Account => a })
  }

  When(s"""^Users $stringValuePattern names are concatenated in given order$$""") { (csvUids: String) =>
    // this is just for demonstrational purposes... and it is easyer to make the mock with the stored objects
    val userIds = csvUids.split(",").toList
    val users = Await.result(Future.sequence(userIds.map(world.getRecordByIdSync[User](_).toFutureRSP())), timeout.+(1 second))
    val mockCall = MockServiceCall[String, String]({ _ => users.foldLeft("") { case (a, c) => s"$a${c.name}" } }).doServiceCall(mockServiceCallDelay)

    world.mutateWorld[String, String](mockCall.invoke, "", serviceCallRspId)
  }

  When(s"""^User $stateNamePattern initiate transaction $stateNamePattern from Account $stateNamePattern with amount $stateNamePattern in currency $stringValuePattern$$""") {
    (userId: String, txId: String, accountId: String, amountId: String, currency: String) =>
      type FromAcc = Account
      type ToAcc = Account
      type Amount = Int
      type Currency = String
      type IN = (FromAcc, ToAcc, Amount, Currency)

      val amount = 15 // rnd(0, 100)
      val referencedUser = world.getRecordByIdSync[User](userId).toRSP(timeout.+(1 second))
      val fromAccount = world.getRecordByIdSync[Account](accountId).toRSP(timeout.+(1 second))
      val toAccount = Account("99999999-88888888-77777777", 0, "USD", User("-1", "NA", 0))

      require(fromAccount.owner == referencedUser, s"The referenced user in account [$accountId] (which is [${fromAccount.owner}]) must be the same as [$referencedUser]")

      val mockTxCall = MockServiceCall[IN, Transaction]({ case (fa, ta, a, c) => Transaction(fa, ta, a, c) }).doServiceCall(mockServiceCallDelay)

      world.mutateWorld[IN, Transaction](mockTxCall.invoke, (fromAccount, toAccount, amount, currency), txId)
      world.deriveWorld[Transaction, Amount](txId, amountId, { tx: Transaction => tx.amount })
  }

  Then(s"""^the result is "$stringValuePattern" as String$$""") { (expectedValue: String) =>
    // check the given objects, based on their identifier
    world.checkResult[String]({ reply => reply should ===(expectedValue) }, serviceCallRspId)
  }

  Then(s"""^Transaction $stateNamePattern is accepted if amount $stateNamePattern greater than its amount$$""") {
    (txId: String, limitAmountId: String) =>

      // In this section there are two equivalent solution from testing pov.
      // SOLUTION 1:

      type Amount = Int

      val amount = world.getRecordByIdSync[Amount](limitAmountId).toRSP(timeout.+(1 second))

      world.checkResult[Transaction]({ tx => tx.amount should be < amount }, txId)

      // SOLUTION 2:

      world.checkResult2[Amount, Transaction](limitAmountId, txId) {
        (amount, tx) => tx.amount should be < amount
      }(timeout.+(1 second))

  }

  Then(s"""^Transaction $stateNamePattern is accepted if its amount less than $intValuePattern$$""") { (txId: String, balance: Int) =>
    world.checkResult[Transaction]({ tx => tx.amount should be < balance }, txId)
  }
}

object WordTestingStepDefs {

  // model for testing
  case class User(id: String, name: String, age: Int)

  case class Limit(amount: Int, currency: String, bankName: String)

  case class Account(accounNo: String, balance: Int, currency: String, owner: User)

  case class Transaction(from: Account, to: Account, amount: Int, currency: String)

  // service call mock
  case class MockServiceCall[RQ, RS](response: RQ => RS) {
    def doServiceCall(timeout: Duration = 1 second): ServiceCall[RQ, RS] =
      ServiceCall[RQ, RS] { rq =>
        Future {
          Thread.sleep(timeout.toMillis)
          response(rq)
        }
      }
  }

  def rnd(min: Int, max: Int): Int = {
    val rnd = new scala.util.Random
    min + rnd.nextInt((max - min) + 1)
  }
}