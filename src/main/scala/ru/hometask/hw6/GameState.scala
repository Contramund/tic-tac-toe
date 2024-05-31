package ru.hometask.hw6

import scala.util.Try
import scala.math.max

/** The interface of game state.
  */
trait GameState {

  /** Tries to apply given `cmd` to itself producing new state.
    *
    * @param cmd
    * -- command to apply if possible
    * @return
    *   Either[String, GameState]. It is new state if command is applicable and it is error-string
    *   if command is unknown or invalid.
    */
  def processState(cmd: String): Either[String, GameState]

  /** Checks if game ended.
    *
    * @return
    *   None if game is still in process. Otherwise it returns the name of the winner.
    */
  def isDone: Option[String]

  /** Just visualizes the game state.
    *
    * @return
    *   String.
    */
  def printState: String

  /** Just visualizes the name of player whose turn to go now.
    *
    * @return
    *   String.
    */
  def whichTurn: String
}

/** An abstraction for a linear game field.
  *
  * It's good to have safe type that won't allow you to pass illegal coordinates. For example
  * `LinearCompactSpace` below is an example of bounded integer-valued interval. Very convenient to
  * avoid extra checks in gameLogic.
  *
  * @tparam A
  * -- usually integer, but it can be whatever coordinates you need for your game.
  */
trait Space[A] {

  /** Collects all possible coordinates in a list.
    *
    * It is mainly used to print the field of this type. There seems to be no other way of cycling
    * trough all possible coordinates. It could also be an iterator, but anyway I do not use
    * iterators for this trait.
    *
    * @return
    *   List of all coordinates.
    */
  def range(): List[A]

  /** Parses the coordinate.
    *
    * We need a way to obtain a coordinate from a string when we try to parse player's step.
    *
    * @param s
    *   String.
    * @return
    *   The actual coordinate value or an error-string which may provide some hints to user.
    */
  def fromString(s: String): Either[String, A]
}

/** Implements Tic-Tac-Toe game over 2-dimensional free space on A (field AxA).
  *
  * @param fields
  * -- a map that store player's marks based on their type-save coordinates.
  * @param step
  * -- the counter of players' steps.
  * @param space$A$0
  * -- imports implicit realization. By default we use `LinearCompactSpace`.
  * @tparam A
  * -- a type that implements Space trait that allow us to iterate through this space and visualize
  * it or check some predicates.
  */
class TTTGameState[A: Space](fields: Map[(A, A), Char]) extends GameState {
  private def defaultChar: Char = '⬛'
  private def stepToChar(i: Int): Char =
    if (i % 2 == 0) 'x' else '◯' // '⚫', '❌' and '✕' are also available
  private def stepToPlayer(step: Int) = s"Player-${step % 2 + 1} (${stepToChar(step)})"

  override def whichTurn: String = stepToPlayer(fields.size)

  override def printState: String = {
    val coordinateRange = implicitly[Space[A]].range()
    coordinateRange.reverse
      .map { y =>
        coordinateRange.map(x => fields.getOrElse((x, y), defaultChar)).mkString
      }
      .mkString("\n")
  }

  override def processState(cmd: String): Either[String, GameState] =
    cmd
      .split(' ')
      .map(implicitly[Space[A]].fromString(_)) match {
      case arr if arr.length != 2 =>
        Left(s"Expected exactly 2 natural numbers divided by <SPACE>, got ${arr.length}")
      case Array(Left(x), _) => Left(s"Cannot parse first coordinate: $x")
      case Array(_, Left(y)) => Left(s"Cannot parse second coordinate: $y")
      case Array(Right(x), Right(y)) if fields.contains((x, y)) => Left(s"Field ${(x, y)} is taken")
      case Array(Right(x), Right(y)) =>
        Right(new TTTGameState[A](fields + ((x, y) -> stepToChar(fields.size))))
      // $COVERAGE-OFF$
      // magic impossible case that solvers are failed to proof non-existent
      case what =>
        Left(
          s"Please report this message to @Contramund in telegram: ${what.mkString("Array(", ", ", ")")}",
        )
      // $COVERAGE-ON$
    }

  override def isDone: Option[String] = {
    val coordinateRange = implicitly[Space[A]].range()
    val playerChar = stepToChar(fields.size - 1)

    val rawCondition = coordinateRange.exists { y =>
      coordinateRange.forall(x => fields.getOrElse((x, y), defaultChar) == playerChar)
    }
    val columnCondition = coordinateRange.exists { x =>
      coordinateRange.forall(y => fields.getOrElse((x, y), defaultChar) == playerChar)
    }
    val firstCrossCondition = coordinateRange.forall { x =>
      fields.getOrElse((x, x), defaultChar) == playerChar
    }
    val secondCrossCondition = coordinateRange.zip(coordinateRange.reverse).forall { xx =>
      fields.getOrElse(xx, defaultChar) == playerChar
    }

    if (columnCondition || rawCondition || firstCrossCondition || secondCrossCondition)
      Some(s"${stepToPlayer(fields.size - 1)} won!!!")
    else if (fields.size == coordinateRange.size * coordinateRange.size)
      Some("It's a draw!!!")
    else
      None
  }
}

/** Provides implicit bounded linear space for base-class and default state based on this space.
  */
object GameStateTTT {
  class LinearCompactSpace(l: Int, r: Int) extends Space[Int] {
    override def fromString(s: String): Either[String, Int] =
      Try(s.toInt).toOption
        .toRight(s"Cannot parse \"$s\".")
        .flatMap(i => if (i >= l) Right(i) else Left(s"Expected integer greater than $l"))
        .flatMap(i => if (i <= r) Right(i) else Left(s"Expected integer less than $r"))

    override def range(): List[Int] = (l to r).toList
  }

  private val fieldSize =
    max(sys.env.get("fieldSize").flatMap(s => Try(s.toInt).toOption).getOrElse(3), 2)
  implicit val defaultField: Space[Int] = new LinearCompactSpace(1, fieldSize)
  val defaultGameState: TTTGameState[Int] = new TTTGameState[Int](Map())
}
