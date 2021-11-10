import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._

class EvalSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure eval works as expected" in {
      val eagerEval = Eval.now {
        println("This is eagerly evaluated")
        1 :: 2 :: 3 :: Nil
      }
      eagerEval.value mustEqual(List[Int](1,2,3))
    }
  }
}
