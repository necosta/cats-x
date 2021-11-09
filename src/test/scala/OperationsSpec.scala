import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import cats.Semigroup

import org.specs2.mutable.Specification

class OperationsSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure cats semigroup combines values - 1" in {
      Semigroup[Int].combine(1, 2) mustEqual 3
      Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) mustEqual List(1,2,3,4,5,6)
      Semigroup[Option[Int]].combine(Option(1), Option(2)) mustEqual(Option(3))
      Semigroup[Option[Int]].combine(Option(1), None) mustEqual(Option(1))
      Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) mustEqual(67)
    }
  }

  "scala-examples" should {
    "make sure cats semigroup combines values - 2" in {
      val aMap = Map("foo" -> Map("bar" -> 5))
      val anotherMap = Map("foo" -> Map("bar" -> 6))
      val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

      combinedMap.get("foo") mustEqual(Some(Map("bar" -> 11)))
    }
  }

  "scala-examples" should {
    "make sure cats semigroup combines values - 3" in {
      import cats.implicits.catsSyntaxSemigroup

      val one: Option[Int] = Option(1)
      val two: Option[Int] = Option(2)
      val n: Option[Int] = None

      one |+| two mustEqual(Option(3))
      n |+| two mustEqual(Option(2))
      n |+| n mustEqual(None)
      two |+| n mustEqual(Option(2))
    }
  }

}
