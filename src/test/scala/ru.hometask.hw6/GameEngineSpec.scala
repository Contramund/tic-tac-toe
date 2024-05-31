package ru.hometask.hw6

import cats.Show
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

/** Mock for IO implementation
  *
  * It basically just takes list of consecutive inputs and gives them as a results of readLine
  * operations. All print/println operations are saved inside this class to later checks.
  *
  * @param ans
  *   just a list of answers for consecutive readLine() calls
  */
class IOMock(ans: List[String]) extends IConsole[Option] {
  var toRead: List[String] = ans
  var hist: List[String] = List()

  override def print[A: Show](s: A): Option[Unit] = {
    hist = s.toString :: hist
    Some(())
  }

  override def println[A: Show](s: A): Option[Unit] = {
    hist = s.toString + "\n" :: hist
    Some(())
  }

  override def readLine: Option[String] = {
    val res = toRead.head
    toRead = toRead.tail
    hist = s"<UserInput=$res>\n" :: hist
    Some(res)
  }
}

class GameEngineSpec extends AnyFlatSpec with Matchers with MockFactory {

  "GameEngineSession" should "end when logic is done and retry if invalid command provided " in {
    val GameStateMock: GameState = mock[GameState]
    (() => GameStateMock.whichTurn).expects().returning("<PlayerName>").repeated(1): Unit
    (() => GameStateMock.printState).expects().returning("<GameStatePrinted>").repeated(3): Unit
    (GameStateMock.processState _)
      .expects("InvalidCommand")
      .returning(Left("<SomeInnerError>")): Unit
    (GameStateMock.processState _).expects("ValidCommand").returning(Right(GameStateMock)): Unit
    (() => GameStateMock.isDone).expects().returning(Some("<GameEndedMsg>")): Unit

    implicit val mockForIo: IConsole[Option] = new IOMock(List("InvalidCommand", "ValidCommand"))

    (new GameEngine[Option]).gameSession(GameStateMock, "<EntryMsg>"): Unit

    mockForIo.asInstanceOf[IOMock].hist.reverse.fold("")(_ + _) shouldBe """
Current state:
<GameStatePrinted>
:: <EntryMsg>
~> <UserInput=InvalidCommand>

Current state:
<GameStatePrinted>
:: Input error: <SomeInnerError>.
Dear <PlayerName>, your turn! (type "help" to read manual)
~> <UserInput=ValidCommand>


WINNER WINNER, CHICKEN DINNER!!!
<GameStatePrinted>

<GameEndedMsg>

"""
  }

  "GameEngineSession" should "show cli commands" in {
    val GameStateMock: GameState = mock[GameState]
    (() => GameStateMock.whichTurn).expects().returning("<PlayerName>").repeated(1): Unit
    (() => GameStateMock.printState).expects().returning("<GameStatePrinted>").repeated(2): Unit

    implicit val mockForIo: IConsole[Option] = new IOMock(List("help", "exit"))

    (new GameEngine[Option]).gameSession(GameStateMock, "<EntryMsg>"): Unit

    mockForIo.asInstanceOf[IOMock].hist.reverse.fold("")(_ + _) shouldBe """
Current state:
<GameStatePrinted>
:: <EntryMsg>
~> <UserInput=help>

Current state:
<GameStatePrinted>
:: Just play the game, OK?
Dear <PlayerName>, your turn! (type "help" to read manual)
~> <UserInput=exit>

Game ended by user's intention

"""
  }

  "GameEngineRestart" should "ask for retry if couldn't parse" in {
    implicit val mockForIo: IConsole[Option] = new IOMock(List("help", "n"))

    (new GameEngine[Option]).askRestart() shouldBe Some(false): Unit

    mockForIo.asInstanceOf[IOMock].hist.reverse.fold("")(_ + _) shouldBe
      """Another game, dudes?[Y/n]: <UserInput=help>
Unknown answer: "help"
Another game, dudes?[Y/n]: <UserInput=n>
"""
  }

  "GameEngineRestart" should "ask for retry if got nothing" in {
    implicit val mockForIo: IConsole[Option] = new IOMock(List("", "Y"))

    (new GameEngine[Option]).askRestart() shouldBe Some(true): Unit

    mockForIo.asInstanceOf[IOMock].hist.reverse.fold("")(_ + _) shouldBe
      """Another game, dudes?[Y/n]: <UserInput=>
Unknown answer: ""
Another game, dudes?[Y/n]: <UserInput=Y>
"""
  }

  "GameEngine" should "restart game sessions when needed" in {
    val GameStateMock: GameState = mock[GameState]
    (() => GameStateMock.whichTurn).expects().returning("<PlayerName>").repeated(2): Unit
    (() => GameStateMock.printState).expects().returning("<GameStatePrinted>").repeated(2): Unit
//    (GameStateMock.processState _).expects("InvalidCommand").returning(Left("<SomeInnerError>")): Unit
//    (GameStateMock.processState _).expects("ValidCommand").returning(Right(GameStateMock)): Unit
//    (() => GameStateMock.isDone).expects().returning(Some("<GameEndedMsg>")): Unit

    implicit val mockForIo: IConsole[Option] = new IOMock(List("exit", "y", "exit", "n"))

    (new GameEngine[Option]).run(GameStateMock): Unit

    mockForIo.asInstanceOf[IOMock].hist.reverse.fold("")(_ + _) shouldBe """
Current state:
<GameStatePrinted>
:: All right boys, let us go...
Dear <PlayerName>, your turn! (type "help" to read manual)
~> <UserInput=exit>

Game ended by user's intention

Another game, dudes?[Y/n]: <UserInput=y>

Current state:
<GameStatePrinted>
:: All right boys, let us go...
Dear <PlayerName>, your turn! (type "help" to read manual)
~> <UserInput=exit>

Game ended by user's intention

Another game, dudes?[Y/n]: <UserInput=n>
"""
  }
}
