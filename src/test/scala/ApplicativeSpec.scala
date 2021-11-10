import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats.*

class ApplicativeSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure applicative is pure" in {
      Applicative[Option].pure(1) mustEqual(Some(1))
      Applicative[List].pure(1) mustEqual(List(1))
      Applicative[Seq].pure(1) mustEqual(Seq(1))

      (Applicative[List] compose Applicative[Option]).pure(1) mustEqual(List(Some(1)))
    }
  }
}
