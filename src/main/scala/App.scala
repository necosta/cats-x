import cats.effect.{IO, IOApp}

object App extends IOApp.Simple {
  val run = IO.println("Hello, World!")
}