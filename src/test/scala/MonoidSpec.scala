import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._
import cats.implicits._

class MonoidSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure monoids combine" in {
      Monoid[String].empty mustEqual("")
      Monoid[String].combineAll(List("a", "b", "c")) mustEqual("abc")
      Monoid[String].combineAll(List()) mustEqual("")
    }
    "make sure monoids combine all" in {
      Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) mustEqual(Map("a" -> 4, "b" -> 2))
      Monoid[Map[String, Int]].combineAll(List()) mustEqual(Map())
    }
    "make sure monoids fold map" in {
      val list = List(1, 2, 3, 4, 5)
      list.foldMap(identity) mustEqual(15)
      list.foldMap(i => i.toString) mustEqual("12345")
    }
  }
}
