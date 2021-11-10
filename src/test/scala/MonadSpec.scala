import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification
import cats.*
import cats.data.OptionT

import scala.annotation.tailrec

class MonadSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure monad flattens" in {
      Option(Option(1)).flatten mustEqual(Some(1))
      Option(None).flatten mustEqual(None)
      // ToDo: Investigate why type inference does not work
      List(List(1), List(2, 3)).flatten[Int] mustEqual(List(1, 2, 3))
      List(List("a"), List("b", "c")).flatten[String] mustEqual(List("a", "b", "c"))
    }
    "make sure monad flat maps" in {
      Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) mustEqual(List(1, 1, 2, 2, 3,3))
    }
    "make sure monad uses ifM" in {
      Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) mustEqual(Some("truthy"))
      Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) mustEqual(List(1, 2, 3, 4, 1, 2))
    }
    "make sure monad uses optionTMonad" in {
      import cats.implicits._

      implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[OptionT[F, *]] =
        new Monad[OptionT[F, *]] {
          def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
          def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
            OptionT {
              F.flatMap(fa.value) {
                case None => F.pure(None)
                case Some(a) => f(a).value
              }
            }
          def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] = ???
        }

      optionTMonad[List].pure(42) mustEqual(OptionT(List(Some(42))))
    }
  }
}
