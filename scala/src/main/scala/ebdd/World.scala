package ebdd

import cats.data._
import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.Assertion
import org.scalatest.Assertions.fail
import ebdd.CategoryConverters._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

// TODO: unify cucumber references in different projects (!) - cucumberLibs
// TODO: update cucumber libs to the newest releases (2.0.1 -> 4.2.0 / 2.4.0 -> 4.2.2)
class World {

  // WARNING: immutable.List is thread safe by default. However parallel execution can
  //          cause still problems, since there can be race condition between parallely
  //          executed assignments.
  //          See: AsyncWordSpec which is used in the tests.
  var history = List[RequestResponse[_, _]]()
  val history2 = Ref.of[IO, List[RequestResponse[_, _]]](List.empty[RequestResponse[_, _]]).unsafeRunSync()

  type ET[T] = Either[Throwable, T]
  type FE[T] = EitherT[Future, Throwable, T]
  type SCF[IN, OUT] = IN => Future[OUT]

  /**
   * History record.
   *
   * @param serviceCall service call
   * @param request request parameter
   * @param response the response of the service call
   * @param fId identifer
   * @tparam RQ type of the request
   * @tparam RSP type of the reponse
   */
  case class RequestResponse[RQ, RSP](
    serviceCall: SCF[RQ, RSP],
    request: RQ,
    response: FE[RSP],
    fId: PartialFunction[RequestResponse[RQ, RSP], Future[String]]) {

    def evaluateIds(): Future[(RequestResponse[RQ, RSP], String)] = fId(this).map(r => (this, r))

    def toFutureRSP: Future[RSP] = this.response.toFuture()

    def toRSP(timeout: Duration = 3 seconds): RSP = Await.result(toFutureRSP, timeout)
  }

  //  private val logger = LoggerFactory.getLogger(this.getClass)
  //  val kafkaTestUtils = new KafkaTestUtils

  type HRecId[RQ, RSP] = PartialFunction[RequestResponse[RQ, RSP], Future[String]]

  /**
   * A new operation, the request, response and its identifier can be saved using this function.
   *
   * @param srvCall function of the service, which is [[ RQ => Future[RSP] ]]
   * @param request request, which will be used, when the `srvCall` service API is called
   * @param constId identifier, which will identify the history record. [[ RequestResponse[_,_] ]]
   * @tparam RQ type of the request
   * @tparam RSP type of the response
   * @return with the result of the computation as an instance of [[RSP]]
   */
  def mutateWorld[RQ, RSP](srvCall: SCF[RQ, RSP], request: RQ, constId: String): Future[RSP] = {
    val idFunction: HRecId[RQ, RSP] = { case _ => Future.successful(constId) }
    updateWorld[RQ, RSP](srvCall, request, idFunction)
  }

  /**
   * A new operation, the request, response and its identifier can be saved using this function.
   *
   * @param srvCall function of the service, which is [[ RQ => Future[RSP] ]]
   * @param request request, which will be used, when the `srvCall` service API is called
   * @param idFunction function, which is used for identification of the history record. When a stored record is about
   *                   to reach out to be used, this method will be called and evaluated. The result of this method
   *                   will be the identifier what one would looking for.
   * @tparam RQ type of the request
   * @tparam RSP type of the response
   * @return with the result of the computation as an instance of [[RSP]]
   */
  def updateWorld[RQ, RSP](srvCall: SCF[RQ, RSP], request: RQ, idFunction: HRecId[RQ, RSP]): Future[RSP] = {
    // todo: state handling could be externalized into an actor, and concurrency issues are sorted... pls consider.
    synchronized {
      val response = srvCall(request)
      val futureither = EitherT(response.map(Right(_)) recover { case t => Left(t) })
      history = history :+ RequestResponse(srvCall, request, futureither, idFunction)
      history2.update(_ :+ RequestResponse(srvCall, request, futureither, idFunction))
      response
    }
  }

  /**
   * This method can extend the history with new RequestResponse record, which is derived a formerly stored
   * one, which is identified by the {{{rpsId}}}. The new record's id will be {{{dRspId}}}. Its value will
   * be calculated with the help of the function {{{f}}}.
   *
   * @param rpsId stored entity's id
   * @param dRspId entity which will be derived from a formerly stored record.
   * @param f value generator value
   * @tparam RSP original entity's type
   * @tparam DRSP generated entity's type
   * @return [[Future]] value of the newly created entity, which will be stored in history.l
   */
  def deriveWorld[RSP, DRSP](rpsId: String, dRspId: String, f: RSP => DRSP): Future[DRSP] = {
    val idFunction: HRecId[String, DRSP] = { case _ => Future.successful(dRspId) }
    updateWorldDerived[RSP, DRSP](rpsId, idFunction, f)
  }

  def updateWorldDerived[RSP, DRSP](heId: String, id: HRecId[String, DRSP], f: RSP => DRSP): Future[DRSP] = {
    val srvCall: SCF[String, DRSP] = { in =>
      getHistoryRecordById[RSP](in)
        .map(_.response.toFuture(f))
        .flatten
    }

    updateWorld[String, DRSP](srvCall, heId, id)
  }

  @Deprecated
  def updateWorld[RQ, RSP](implicit srvCall: SCF[RQ, RSP], request: RQ): Future[RSP] = {

    val response = srvCall(request)
    val futureither: FE[RSP] = EitherT(response.map(Right(_)) recover { case t => Left(t) })

    // todo: update properly
    val idFunction: HRecId[RQ, RSP] = { case _: RequestResponse[RQ, RSP] => Future.successful(s"auto-${history.size}") }
    history = history :+ RequestResponse(srvCall, request, futureither, idFunction)
    history2.update(_ :+ RequestResponse(srvCall, request, futureither, idFunction))

    response
  }

  @Deprecated
  def defaultFallback[RSP](rr: RequestResponse[_, RSP], atMost: Duration): PartialFunction[FE[RSP], Assertion] = {
    case fe: FE[RSP] =>
      Await.result(fe.value, atMost) match {
        case Right(rsp) => fail(s"No error, but unexpected result. Value: Success(Right($rsp))")
        case Left(t) => fail(s"Execution Error. Func: [${rr.serviceCall}] Rq: [${rr.request}] Cause: [${t.getMessage}]")
      }
  }

  @Deprecated
  def checkHistoryRegistry[RSP](check: RequestResponse[_, RSP] => Assertion)(nth: Int = 1): Assertion = {
    history.takeRight(nth).headOption match {
      case Some(rr) => check(rr.asInstanceOf[RequestResponse[_, RSP]])
      case None => fail("There's no data in World's history.")
    }
  }

  @Deprecated
  def checkHistoryRegistryWithDefaultFallbackAsync[RSP](pf: PartialFunction[ET[RSP], Assertion], nth: Int = 1)(atMost: Duration = 1 second): Assertion = {
    def helper: PartialFunction[FE[RSP], Assertion] = {
      case fin: FE[RSP] => pf(Await.result(fin.value, atMost))
    }
    checkHistoryRegistry[RSP](rr => (helper orElse defaultFallback(rr, atMost))(rr.response))(nth)
  }

  private def fallback[RSP]: PartialFunction[ET[RSP], ET[Assertion]] = {
    case Right(r) => fail(s"Unexpected result. Value: [$r]")
    case Left(t) => fail(s"Execution Error. Msg: [${t.getMessage}]", t)
  }

  /**
   * This method runs the check `testFn` against a given result identified by `historyRecordId`.
   * The execution is asynchronous, therefore the check either will run against the results or
   * timeout will happen, which defined by `atMost`.
   *
   * @param testFn runs when the result is available (the future is complete).
   *               PartialFunction[Either[Throwable, RSP], Assertion] type here must be kept.
   * @param historyRecordId the identifier, which identify the record in history.
   * @param atMost timeout value, how long the result is waited.
   * @tparam RSP type of the result. srvFn: RQ => Future[RSP]
   * @return the result of the assertion defined by `testFn`
   */
  def checkResult[RSP](testFn: RSP => Assertion, historyRecordId: String)(implicit atMost: Duration = 3 seconds): Assertion = {

    def pfWrapperOfTestFn: PartialFunction[ET[RSP], Assertion] = {
      case Right(response) => testFn(response)
    }

    val compareExpectation: PartialFunction[ET[RSP], ET[Assertion]] = {
      case e =>
        Try(pfWrapperOfTestFn(e)) match {
          case Success(r) => Right(r)
          case Failure(t) => Left(t)
        }
    }

    val assertionResult = getHistoryRecordById[RSP](historyRecordId)
      .flatMap(_.response.value.map((compareExpectation orElse fallback[RSP])(_)))
      .map {
        case Right(r) => r
        case Left(t) => fail(s"Exception were happened: [${t.getMessage}]")
      }

    Await.result[Assertion](assertionResult, atMost)
  }

  private def syncTesterWithParamsList(paramsList: List[Future[RequestResponse[_, _]]], testFn: PartialFunction[List[Any], Assertion])(implicit atMost: Duration): Assertion = {
    val responses = Future.sequence(paramsList)
      .map { l =>
        Future.sequence {
          l.map(_.response.toFuture())
        }
      }.flatten

    val fb: PartialFunction[List[_], Assertion] = {
      case anyList => fail(s"Test function were not decisive. Fallback's result is default error. [$anyList]")
    }

    val f = responses.map {
      case list if list.size == paramsList.size => (testFn orElse fb) (list)
      case msg => fail(s"Unexpected number of parameters. [$msg]")
    }

    Await.result(f, atMost)
  }

  def checkResult1[T0](t0id: String)(testFn: T0 => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(getHistoryRecordById[T0](t0id))
    syncTesterWithParamsList(paramsList, { case h0 :: _ => testFn(h0.asInstanceOf[T0]) })
  }

  def checkResult2[T0, T1](t0id: String, t1id: String)(testFn: (T0, T1) => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(getHistoryRecordById[T0](t0id), getHistoryRecordById[T1](t1id))
    syncTesterWithParamsList(paramsList, { case h0 :: h1 :: _ => testFn(h0.asInstanceOf[T0], h1.asInstanceOf[T1]) })
  }

  def checkResult3[T0, T1, T2](t0id: String, t1id: String, t2id: String)(testFn: (T0, T1, T2) => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(
      getHistoryRecordById[T0](t0id),
      getHistoryRecordById[T1](t1id),
      getHistoryRecordById[T2](t2id))
    syncTesterWithParamsList(paramsList, { case h0 :: h1 :: h2 :: _ => testFn(h0.asInstanceOf[T0], h1.asInstanceOf[T1], h2.asInstanceOf[T2]) })
  }

  def checkResult4[T0, T1, T2, T3](t0id: String, t1id: String, t2id: String, t3id: String)(testFn: (T0, T1, T2, T3) => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(
      getHistoryRecordById[T0](t0id),
      getHistoryRecordById[T1](t1id),
      getHistoryRecordById[T2](t2id),
      getHistoryRecordById[T3](t3id))
    syncTesterWithParamsList(paramsList, {
      case h0 :: h1 :: h2 :: h3 :: _ =>
        testFn(h0.asInstanceOf[T0], h1.asInstanceOf[T1], h2.asInstanceOf[T2], h3.asInstanceOf[T3])
    })
  }

  def checkResult5[T0, T1, T2, T3, T4](t0id: String, t1id: String, t2id: String, t3id: String, t4id: String)(testFn: (T0, T1, T2, T3, T4) => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(
      getHistoryRecordById[T0](t0id),
      getHistoryRecordById[T1](t1id),
      getHistoryRecordById[T2](t2id),
      getHistoryRecordById[T3](t3id),
      getHistoryRecordById[T4](t4id))
    syncTesterWithParamsList(paramsList, {
      case h0 :: h1 :: h2 :: h3 :: h4 :: _ =>
        testFn(h0.asInstanceOf[T0], h1.asInstanceOf[T1], h2.asInstanceOf[T2], h3.asInstanceOf[T3], h4.asInstanceOf[T4])
    })
  }

  def checkResult6[T0, T1, T2, T3, T4, T5](t0id: String, t1id: String, t2id: String, t3id: String, t4id: String, t5id: String)(testFn: (T0, T1, T2, T3, T4, T5) => Assertion)(implicit atMost: Duration = 3 seconds): Assertion = {
    val paramsList = List(
      getHistoryRecordById[T0](t0id),
      getHistoryRecordById[T1](t1id),
      getHistoryRecordById[T2](t2id),
      getHistoryRecordById[T3](t3id),
      getHistoryRecordById[T4](t4id),
      getHistoryRecordById[T5](t5id))
    syncTesterWithParamsList(paramsList, {
      case h0 :: h1 :: h2 :: h3 :: h4 :: h5 :: _ =>
        testFn(h0.asInstanceOf[T0], h1.asInstanceOf[T1], h2.asInstanceOf[T2], h3.asInstanceOf[T3], h4.asInstanceOf[T4], h5.asInstanceOf[T5])
    })
  }

  /**
   * Get [[ RequestResponse[_, RSP] ]] asynchronously from history based on its id.
   *
   * @param historyRecordId identifier of the record. Expected,
   * @tparam RSP RSP the expected return type
   * @return with the object, which was found. Errors handled by Try.
   */
  def getHistoryRecordById[RSP](historyRecordId: String): Future[RequestResponse[_, RSP]] = {
    val rrWithIds: List[(Future[(World#RequestResponse[_, _], String)])] = history.map(_.evaluateIds())
    val recordsToEvaluate = Future.sequence(rrWithIds).map(_.filter(_._2 == historyRecordId))
    recordsToEvaluate
      .flatMap[RequestResponse[_, RSP]] { // [List[(World#RequestResponse[_, _], String)] => RequestResponse[_, RSP]]
        case Nil => Future.failed(new RuntimeException(s"no relevant record were found in history from [${history.size}] record with id [$historyRecordId]"))
        case (rr, _) :: Nil => Future.successful(rr.asInstanceOf[RequestResponse[_, RSP]])
        case list => Future.failed(new RuntimeException(s"[${list.size}] records were found in history from [${history.size}] record with id [$historyRecordId]"))
      }
  }

  /**
   * Synchronous call to get a record from the history identified by [historyRecordId].
   *
   * @param historyRecordId identifier of the record.
   * @param atMost duration which specifies the timeout to wait to finish all the service call Futures in history.
   * @tparam RSP RSP the expected return type
   * @return with the object, which was found. Errors handled by Try.
   */
  def getRecordByIdSync[RSP](historyRecordId: String)(implicit atMost: Duration): RequestResponse[_, RSP] = {
    Await.result(getHistoryRecordById[RSP](historyRecordId), atMost)
  }

  /**
   * Asynchronous call to get a list of records from the history identified by historyRecordIds.
   *
   * @param listOfHistoryRecIds identifier of the record.
   * @tparam RSP RSP the expected return type
   * @return with the objects, which were found. Errors handled by Try.
   */
  def getHistoryRecordsByIds[RSP](listOfHistoryRecIds: List[String]): Future[List[RequestResponse[_, RSP]]] = {
    Future.sequence { listOfHistoryRecIds.map(getHistoryRecordById[RSP](_)) }
  }

  /**
   * Synchronous call to get a record from the history identified by listOfHistoryRecIds.
   *
   * @param listOfHistoryRecIds list of the ids, that are looked after in the state (history)
   * @param atMost duration which specifies the timeout to wait to finish all the service call Futures in history.
   * @tparam RSP RSP the expected return type
   * @return with list of objects, which were found. Errors handled by Try.
   */
  def getHistoryRecordsByIdsSync[RSP](listOfHistoryRecIds: List[String])(implicit atMost: Duration): List[RequestResponse[_, RSP]] = {
    Await.result(getHistoryRecordsByIds[RSP](listOfHistoryRecIds), atMost)
  }

  /**
   * Get the last `RequestResponse[_, Response]` item, which was saved into history.
   *
   * @tparam Response Type of the expected result type.
   * @return returns with history record.
   */
  def getLastElement[Response](): RequestResponse[_, Response] = {
    history.head.asInstanceOf[RequestResponse[_, Response]]
  }

  def updateWorldWithITCall[Request, Response](itCall: Response, request: Request): Future[Response] = {
    updateWorld({ _: Request => Future.successful(itCall) }, request)
  }

  /**
   * Clear history from all records it contains.
   *
   * @return the new, empty history
   */
  def clear(): List[RequestResponse[_, _]] = {
    history = List[RequestResponse[_, _]]()
    history
  }
}
