import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._

class FunctorSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure functors map" in {
      Functor[List].map(List("qwer", "adsfg"))(_.length) mustEqual(List(4,5))
      Functor[Option].map(Option("Hello"))(_.length) mustEqual(Option(5))
      Functor[Option].map(None: Option[String])(_.length) mustEqual(None)
      val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
      lenOption(Some("Hello")) mustEqual(Option(5))
    }
    "make sure functors fproduct" in {
      val source = List("Cats", "is", "awesome")
      val product = Functor[List].fproduct(source)(_.length).toMap

      product.get("Cats").getOrElse(0) mustEqual(4)
      product.get("is").getOrElse(0) mustEqual(2)
      product.get("awesome").getOrElse(0) mustEqual(7)
      product.get("boo").getOrElse(0) mustEqual(0)
    }
    "make sure functors compose" in {
      val listOpt = Functor[List] compose Functor[Option]
      listOpt.map(List(Some(1), None, Some(3)))(_ + 1) mustEqual(List(Some(2), None, Some(4)))
    }
  }
}
