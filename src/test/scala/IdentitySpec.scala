import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._

class IdentitySpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure identity" in {
      val anId: Id[Int] = 42
      anId mustEqual(42)
      Applicative[Id].pure(42) mustEqual(42)
    }
    "make sure identity coflats" in {
      val fortytwo: Int = 42
      Comonad[Id].coflatMap(fortytwo)(_ + 1) mustEqual(43)
    }
  }
}
