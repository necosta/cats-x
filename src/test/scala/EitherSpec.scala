import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

import cats._
import cats.implicits._

class EitherSpec extends Specification with CatsEffect {
  "scala-examples" should {
    "make sure either works as expected" in {
      val right: Either[String, Int] = Either.right(5)
      right.map(_ + 1) mustEqual(Right(6))

      val left: Either[String, Int] = Either.left("Something went wrong")
      left.map(_ + 1) mustEqual(Left("Something went wrong"))
    }
    "make sure either style works" in {
      object EitherStyle {
        def parse(s: String): Either[NumberFormatException, Int] =
          if (s.matches("-?[0-9]+")) Either.right(s.toInt)
          else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

        def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
          if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
          else Either.right(1.0 / i)

        def stringify(d: Double): String = d.toString

        def magic(s: String): Either[Exception, String] =
          parse(s).flatMap(reciprocal).map(stringify)
      }
      EitherStyle.parse("Not a number").isRight mustEqual(false)
      EitherStyle.parse("2").isRight mustEqual(true)

      EitherStyle.magic("0").isRight mustEqual(false)
      EitherStyle.magic("1").isRight mustEqual(true)
      EitherStyle.magic("Not a number").isRight mustEqual(false)

      val result = EitherStyle.magic("2") match {
        case Left(_: NumberFormatException) => "Not a number!"
        case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
        case Left(_) => "Unknown error"
        case Right(result) => s"Got reciprocal: ${result}"
      }
      result mustEqual("Got reciprocal: 0.5")
    }
    "make sure either left maps" in {
      val right: Either[String, Int] = Right(41)
      right.map(_ + 1) mustEqual(Right(42))

      val left: Either[String, Int] = Left("Hello")
      left.map(_ + 1) mustEqual(Left("Hello"))
      left.leftMap(_.reverse) mustEqual(Left("olleH"))
    }
    "make sure either asleft/asRight" in {
      val right: Either[String, Int] = 42.asRight[String]
      right mustEqual(Right(42))
      val left: Either[String, Int] = "hello 🐈s".asLeft[Int]
      left mustEqual(Left("hello 🐈s"))
    }
  }
}
