package ebdd

import cats.data.EitherT
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CategoryConverters {
  implicit class EitherTConverter[A](et: EitherT[Future, Throwable, A]) {
    def toFuture(): Future[A] = {
      toFuture(None).map(_.left.get)
    }

    def toFuture[T](f: A => T): Future[T] = {
      toFuture(Some(f)).map(_.right.get)
    }

    private def toFuture[T](sf: Option[A => T]): Future[Either[A, T]] = {
      et.value.map {
        _ match {
          case Right(v) if sf.isDefined => Future.successful(Right(sf.get(v)))
          case Right(v) => Future.successful(Left(v))
          case Left(t) => Future.failed(t)
        }
      }.flatten
    }
  }
}
