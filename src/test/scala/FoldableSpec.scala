import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._
import cats.implicits._

class FoldableSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure foldable folds left" in {
      Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) mustEqual(6)
      Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) mustEqual("abc")
    }
    "make sure foldable folds right" in {
      val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) => Later(x + rest.value))
      lazyResult.value mustEqual(6)
    }
    "make sure foldable folds" in {
      Foldable[List].fold(List("a", "b", "c")) mustEqual("abc")
      Foldable[List].fold(List(1, 2, 3)) mustEqual(6)
    }
    "make sure foldable fold maps" in {
      Foldable[List].foldMap(List("a", "b", "c"))(_.length) mustEqual(3)
      Foldable[List].foldMap(List(1, 2, 3))(_.toString) mustEqual("123")
    }
    "make sure foldable fold k" in {
      Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) mustEqual(List(1,2,3,4,5))
      Foldable[List].foldK(List(None, Option("two"), Option("three"))) mustEqual(Some("two"))
    }
    "make sure foldable finds" in {
      Foldable[List].find(List(1, 2, 3))(_ > 2) mustEqual(Some(3))
      Foldable[List].find(List(1, 2, 3))(_ > 5) mustEqual(None)
    }
    "make sure foldable exists" in {
      Foldable[List].exists(List(1, 2, 3))(_ > 2) mustEqual(true)
      Foldable[List].exists(List(1, 2, 3))(_ > 5) mustEqual(false)
    }
    "make sure foldable forall" in {
      Foldable[List].forall(List(1, 2, 3))(_ <= 3) mustEqual(true)
      Foldable[List].forall(List(1, 2, 3))(_ < 3) mustEqual(false)
    }
    "make sure foldable tolist" in {
      Foldable[List].toList(List(1, 2, 3)) mustEqual(List(1, 2, 3))
      Foldable[Option].toList(Option(42)) mustEqual(List(42))
      Foldable[Option].toList(None) mustEqual(List())
    }
    "make sure foldable filters" in {
      Foldable[List].filter_(List(1, 2, 3))(_ < 3) mustEqual(List(1,2))
      Foldable[Option].filter_(Option(42))(_ != 42) mustEqual(List())
    }
    "make sure foldable tranverses" in {
      def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption

      Foldable[List].traverse_(List("1", "2", "3"))(parseInt) mustEqual(Some(()))
      Foldable[List].traverse_(List("a", "b", "c"))(parseInt) mustEqual(None)
    }
    "make sure foldable composes" in {
      val FoldableListOption = Foldable[List].compose[Option]
      FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) mustEqual(10)
      FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) mustEqual("123")
    }
    "make sure foldable performs other ops" in {
      Foldable[List].isEmpty(List(1, 2, 3)) mustEqual(false)
      Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2) mustEqual(List(2,3))
      Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2) mustEqual(List(1))
    }
  }
}
