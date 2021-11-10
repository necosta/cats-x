import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._

class ApplySpec extends Specification with CatsEffect {

  private val addArity2 = (a: Int, b: Int) => a + b
  private val addArity3 = (a: Int, b: Int, c: Int) => a + b + c

  "scala-examples" should {
    "make sure apply map" in {
      val intToString: Int => String = _.toString
      val double: Int => Int = _ * 2
      val addTwo: Int => Int = _ + 2

      Apply[Option].map(Some(1))(intToString) mustEqual(Some("1"))
      Apply[Option].map(Some(1))(double) mustEqual(Some(2))
      Apply[Option].map(None)(addTwo) mustEqual(None)
      Apply[Option].map(Some(1))(addTwo) mustEqual(Some(3))
    }
    "make sure apply compose" in {
      val listOpt = Apply[List] compose Apply[Option]
      val plusOne = (x: Int) => x + 1
      listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) mustEqual(List(Some(2), None, Some(4)))
    }
    "make sure apply ap's" in {
      val intToString: Int => String = _.toString
      val double: Int => Int = _ * 2

      Apply[Option].ap(Some(intToString))(Some(1)) mustEqual(Some("1"))
      Apply[Option].ap(Some(double))(Some(1)) mustEqual(Some(2))
      Apply[Option].ap(Some(double))(None) mustEqual(None)
      Apply[Option].ap(None)(Some(1)) mustEqual(None)
      Apply[Option].ap(Some(intToString))(None) mustEqual(None)
      Apply[Option].ap(None)(None) mustEqual(None)
    }
    "make sure apply apn's" in {

      Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) mustEqual(Some(3))
      Apply[Option].ap2(Some(addArity2))(Some(1), None) mustEqual(None)
      Apply[Option].ap2(Some(addArity2))(None, Some(1)) mustEqual(None)

      Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) mustEqual(Some(6))
    }
    "make sure apply tuples" in {
      Apply[Option].tuple2(Some(1), Some(2)) mustEqual(Some(1, 2))
      Apply[Option].tuple3(Some(1), Some(2), Some(3)) mustEqual(Some(1,2,3))
      // ToDo: Add this example to test?
      Apply[Option].tuple3(Some(1), None, Some(3)) mustEqual(None)
    }
    "make sure apply apN, mapN and tupleN" in {
      import cats.implicits._

      val option2 = (Option(1), Option(2))
      val option3 = (option2._1, option2._2, Option.empty[Int])

      option2 mapN addArity2 mustEqual(Some(3))
      option3 mapN addArity3 mustEqual(None)

      option2 apWith Some(addArity2) mustEqual(Some(3))
      option3 apWith Some(addArity3) mustEqual(None)

      option2.tupled mustEqual(Some(1, 2))
      option3.tupled mustEqual(None)
    }
  }
}
