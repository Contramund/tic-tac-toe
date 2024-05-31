package ru.hometask.hw6

import cats.effect.{ExitCode, IO, IOApp}

// $COVERAGE-OFF$
object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    (new GameEngine[IO]).run(GameStateTTT.defaultGameState)
}
// $COVERAGE-ON$
