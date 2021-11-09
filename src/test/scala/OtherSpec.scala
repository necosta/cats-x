import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

class OtherSpec extends Specification with CatsEffect {
  "my 1st example" should {
    "make sure IO computes the right result" in {
      IO.pure(2).map(_ + 3) flatMap { result =>
        IO(result mustEqual 5)
      }
    }
  }
}
