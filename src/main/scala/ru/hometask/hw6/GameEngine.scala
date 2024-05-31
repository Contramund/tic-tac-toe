package ru.hometask.hw6

import cats.{Monad, Show}
import cats.effect.{ExitCode, IO}
import cats.syntax.flatMap._
import cats.syntax.functor._

/** A trait to get away of using pure IO
  *
  * This trait allow us to execute basic IO-operations and then be able to mock out IO.
  *
  * Unfortunately it seems that cats-effect.IO implements no inner trait and therefore cannot be
  * mocked with standart mock library. This little trait is intended to replace IO with
  * controller-port-like abstraction.
  *
  * @tparam F
  *   can be IO or any other type that have implicit realization of print, println and readLine
  *   functions like IO. In tests I use Option just to get Monad with easy inner-state.
  */
trait IConsole[F[_]] {
  def print[A: Show](s: A): F[Unit]
  def println[A: Show](s: A): F[Unit]
  def readLine: F[String]
}

// $COVERAGE-OFF$

/** Simple ICOnsole realization trough IO
  *
  * It basically just redirects it's calls to IO and therefore can be used as a default (implicit)
  * implementation for GameEngine-IO.
  */
object IConsole {
  private class IOConsole extends IConsole[IO] {
    override def println[A: Show](s: A): IO[Unit] = IO.println(s)
    override def print[A: Show](s: A): IO[Unit] = IO.print(s)
    override def readLine: IO[String] = IO.readLine
  }

  implicit val defaultIOConsole: IConsole[IO] = new IOConsole
}
// $COVERAGE-ON$

/** The main class that holds all the IO operations and command-line handling.
  *
  * The entry point is method `run()`. All the game logic is processed by underlying gameState
  * class. I decided not to store this state in mutable container. Cat-effects lib is expected to
  * optimize game loop as @tailrec.
  */
class GameEngine[F[_]: IConsole: Monad] {
  private def startMsg(): String = "All right boys, let us go..."
  private def helpMsg(): String = "Just play the game, OK?"
  private def printTurn(state: GameState) =
    s"Dear ${state.whichTurn}, your turn! (type \"help\" to read manual)"

  /** Function that decides if the new game session should be started.
    *
    * `askRestart()` method repeatedly ask before player provides recognizable answer. It may be
    * implemented as a tail recursion because cats-effect will likely optimize this recursion. No
    * command lines except answers are accepted because this question is pretty straight-forward.
    *
    * @return
    * -- boolean value:
    *   - true if player wants to play again,
    *   - false otherwise
    */
  def askRestart(): F[Boolean] =
    for {
      _ <- implicitly[IConsole[F]].print("Another game, dudes?[Y/n]: ")
      userAns <- implicitly[IConsole[F]].readLine
      ans <- userAns match {
        case "yes" | "y" | "Y" => implicitly[Monad[F]].pure(true)
        case "no" | "n" | "No" => implicitly[Monad[F]].pure(false)
        case x =>
          for {
            _ <- implicitly[IConsole[F]].println(s"Unknown answer: \"$x\"")
            wantRestart <- askRestart()
          } yield wantRestart
      }
    } yield ans

  /** IO wrapper over game logic.
    *
    * `gameSession(state, msg)` method is implemented trough recursion and cats-effect is expected
    * to optimize it as a tail recursion. This method is called from `run()` method with default
    * game state and some initial message (may be greeting or manual). On each recursive call it
    * shows to user error message from previous step and current game state. Then it processes
    * player's input with gameLogic unless this input is a reserved command like "help" or "exit".
    * At the end of each cycle it eiter call itself to process next state or it prints out the
    * result of the game and returns.
    *
    * @param state
    * -- instance of the GameState class.
    *
    * @param msg
    * -- any message to display to user before it will try to insert it's next command.
    *
    * @return
    * -- IO[Unit]. This method should not fail. In future it might need to return IO[ErrorCode], but
    * now there is no point doing this.
    */
  def gameSession(state: GameState, msg: String): F[Unit] =
    for {
      _ <- implicitly[IConsole[F]].println(s"\nCurrent state:\n${state.printState}")
      _ <- implicitly[IConsole[F]].print(s":: $msg\n~> ")
      cmd <- implicitly[IConsole[F]].readLine
      _ <- cmd.split(' ') match {
        case Array("exit", _*) =>
          for {
            _ <- implicitly[IConsole[F]].println("\nGame ended by user's intention\n")
          } yield ()
        case Array("help", _*) =>
          gameSession(state, s"${helpMsg()}\n${printTurn(state)}")
        case Array("") =>
          gameSession(state, s"Type command pls\n${printTurn(state)}")
        case _ =>
          state.processState(cmd) match {
            case Left(err) =>
              gameSession(state, s"Input error: $err.\n${printTurn(state)}")
            case Right(newState) =>
              newState.isDone match {
                case None => gameSession(newState, printTurn(newState))
                case Some(res) =>
                  for {
                    _ <- implicitly[IConsole[F]].println(
                      s"\n\nWINNER WINNER, CHICKEN DINNER!!!\n${newState.printState}\n\n$res\n",
                    )
                  } yield ()
              }
          }
      }
    } yield ()

  /** The entry point.
    *
    * `run()` method recursively runs gameSession and after each game asks if user wants to play new
    * game.
    *
    * @return
    *   IO[ExitCode]. Now it can only return Success, but for a sake of type consistency it's
    *   allowed to return ExitCode instead of Unit.
    */
  def run(startState: GameState): F[ExitCode] =
    for {
      _ <- gameSession(startState, s"${startMsg()}\n${printTurn(startState)}")
      ans <- askRestart().flatMap(
        if (_) run(startState) else implicitly[Monad[F]].pure(ExitCode.Success),
      )
    } yield ans
}
