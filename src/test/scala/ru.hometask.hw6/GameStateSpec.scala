package ru.hometask.hw6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameStateSpec extends AnyFlatSpec with Matchers {

  "EmptyGameState" should "not be done" in {
    GameStateTTT.defaultGameState.isDone shouldBe None
  }

  "EmptyGameState" should "show which step" in {
    GameStateTTT.defaultGameState.whichTurn shouldBe "Player-1 (x)"
  }

  "CrossedGameState" should "be Done" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
      (2, 2) -> 'x',
      (1, 3) -> '◯',
      (3, 3) -> 'x',
      (1, 4) -> '◯',
      (4, 4) -> 'x',
      (1, 5) -> '◯',
      (5, 5) -> 'x',
    )
    new TTTGameState[Int](map).isDone shouldBe Option("Player-1 (x) won!!!")
  }

  "LinearGameState" should "be Done" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 4)
    val map = Map(
      (1, 1) -> 'x',
      (2, 2) -> '◯',
      (1, 3) -> 'x',
      (1, 2) -> '◯',
      (3, 3) -> 'x',
      (3, 2) -> '◯',
      (4, 4) -> 'x',
      (4, 2) -> '◯',
    )
    new TTTGameState[Int](map).isDone shouldBe Option("Player-2 (◯) won!!!")
  }

  "LinearGameState" should "be printed OK" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 4)
    val map = Map(
      (1, 1) -> 'x',
      (2, 2) -> '◯',
      (1, 3) -> 'x',
      (1, 2) -> '◯',
      (3, 3) -> 'x',
      (3, 2) -> '◯',
      (4, 4) -> 'x',
      (4, 2) -> '◯',
    )

    new TTTGameState[Int](map).printState shouldBe "⬛⬛⬛x\nx⬛x⬛\n◯◯◯◯\nx⬛⬛⬛"
  }

  "GameState.process" should "process allowed commands" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map1 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
    )
    val map2 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
      (2, 2) -> 'x',
    )
    new TTTGameState[Int](map1).processState("2 2") ===
      Right(new TTTGameState[Int](map2))
  }

  "GameState.process" should "fail on attempt to take taken field" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map1 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
    )
    new TTTGameState[Int](map1).processState("1 2") shouldBe Left("Field (1,2) is taken")
  }

  "GameState.process" should "fail to parse first coordinate" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map1 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
    )
    new TTTGameState[Int](map1).processState("lol 2") shouldBe Left(
      "Cannot parse first coordinate: Cannot parse \"lol\".",
    )
  }

  "GameState.process" should "fail to parse second coordinate" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map1 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
    )
    new TTTGameState[Int](map1).processState("2 lol") shouldBe Left(
      "Cannot parse second coordinate: Cannot parse \"lol\".",
    )
  }

  "GameState.process" should "fail to parse when provided with !=2 coordinates" in {
    implicit val currField: Space[Int] = new GameStateTTT.LinearCompactSpace(1, 5)
    val map1 = Map(
      (1, 1) -> 'x',
      (1, 2) -> '◯',
    )
    new TTTGameState[Int](map1).processState("2 3 2") shouldBe
      Left("Expected exactly 2 natural numbers divided by <SPACE>, got 3")
  }

  "LinearSpace" should "parse correct coordinate" in {
    new GameStateTTT.LinearCompactSpace(1, 4).fromString("2") shouldBe Right(2)
  }

  "LinearSpace" should "not parse lower coordinates" in {
    new GameStateTTT.LinearCompactSpace(1, 4).fromString("0") shouldBe Left(
      "Expected integer greater than 1",
    )
  }

  "LinearSpace" should "not parse higher coordinates" in {
    new GameStateTTT.LinearCompactSpace(1, 4).fromString("5") shouldBe Left(
      "Expected integer less than 4",
    )
  }
}
