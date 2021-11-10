import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats.Semigroup
import cats.data.{ NonEmptyList, OneAnd, Validated, ValidatedNel }
import cats.implicits._

class TraverseSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure tranverse works" in {
      def parseIntEither(s: String): Either[NumberFormatException, Int] =
        Either.catchOnly[NumberFormatException](s.toInt)

      def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
        Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

      List("1", "2", "3").traverse(parseIntEither) mustEqual(Right(List(1,2,3)))
      List("1", "abc", "3").traverse(parseIntEither).isLeft mustEqual(true)

      List("1", "2", "3").traverse(parseIntValidated).isValid mustEqual(true)
    }
    "make sure tranverse for effect" in {
      List(Option(1), Option(2), Option(3)).traverse(identity) mustEqual(Some(List(1,2,3)))
      List(Option(1), None, Option(3)).traverse(identity) mustEqual(None)

      List(Option(1), Option(2), Option(3)).sequence_ mustEqual(Some(()))
      List(Option(1), None, Option(3)).sequence_ mustEqual(None)
    }
  }
}
