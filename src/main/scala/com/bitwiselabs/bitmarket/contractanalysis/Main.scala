package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Constants._
import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Main {
  type MoveMap = Map[Move, State]
  type History = List[Move]
  type Payoff = Map[Player, Int]

  val optimalSeq = List(
    EnterDepositB(Bob),
    EnterDepositB(Sam),
    EnterDepositA(Bob),
    EnterDepositA(Sam),
    TransferMoney,
    SignDepositA(Sam),
    SignDepositA(Bob),
    SignDepositB(Sam),
    SignDepositB(Bob),
    StopPlaying(Sam))

  def main(args: Array[String]) {
    val initialState = State(Bob)
    println("Generating game tree...")
    val gameTree = generateGameTree(initialState)
    println("Done")
    println(s"Initial state: $initialState")
    println(s"Desired play moves: ${optimalSeq.mkString("\n ")}")
    println()
    println("Resolving tree...")
    println("Is the desired play moves a dominant strategy? " +
      resolveTree(gameTree, initialState).contains(optimalSeq.drop(0)))
    //println(resolveTree(gameTree, initialState).mkString("\n"))
  }

  def generateGameTree(initialState: State): Map[State, MoveMap] = {
    var result: Map[State, MoveMap] = Map()
    var unprocessedStates: Set[State] = Set(initialState)
    while (unprocessedStates.nonEmpty) {
      val state = unprocessedStates.head
      val moveMap = (for {
        move <- Move.moves
        if move.canPlay(state)
      } yield (move, move(state))).toMap
      val unseenStates = moveMap.values.filterNot(result.keySet.contains).toSet
      unprocessedStates = (unprocessedStates ++ unseenStates) - state
      result = result.updated(state, moveMap)
    }
    result
  }

  val EmptyHistory: History = List()

  def resolveTree(
      tree: Map[State, MoveMap],
      state: State,
      seen: Set[State] = Set()): Map[History, Payoff] = {
    val result: Map[History, Payoff] = if(tree(state).isEmpty) {
      Map(EmptyHistory -> state.payoff)
    } else if (seen.contains(state)) {
      Map(EmptyHistory -> state.payoff)
    } else {
      val moveMap = tree(state)
      val subGamePayoffs = for {
        (move, newState) <- moveMap
      } yield {
        val subGameResolution = resolveTree(tree, newState, seen + state)
        val minPayoff = subGameResolution.values.map(payoff => payoff(state.playerTurn)).min
        (move, subGameResolution) -> minPayoff
      }
      val maxMinPlayerPayoff = subGamePayoffs.maxBy(_._2)._2
      for {
        ((move, resolution), minPayoff) <- subGamePayoffs
        if minPayoff == maxMinPlayerPayoff
        (history, payoff) <- resolution
      } yield (move :: history).toList -> payoff
    }
    result
  }
}
case class State(
    playerTurn: Value,
    payoff: Main.Payoff = Map(Sam -> ValueSam, Bob -> ValueBob),
    paymentMade: Boolean = false,
    depositAEntrances: Set[Player] = Set(),
    depositBEntrances: Set[Player] = Set(),
    depositASignatures: Set[Player] = Set(),
    depositBSignatures: Set[Player] = Set(),
    finished: Boolean = false) {
  val depositAExists = depositAEntrances == Player.values.toSet
  val depositBExists = depositBEntrances == Player.values.toSet
  val otherPlayer = playerTurn match {
    case Sam => Bob
    case Bob => Sam
  }
  def changeTurn = copy(playerTurn = otherPlayer)

  if (depositASignatures.nonEmpty)
    require(depositAExists, this)
  if (depositBSignatures.nonEmpty)
    require(depositBExists, this)
  require(payoff.values.forall(_ >= 0), this)

  override def toString() =
    s"""
      |\tTurn:                          $playerTurn
      |\tPayoff:                        $payoff
      |\tMoney transferred made:        $paymentMade
      |\tPeople who entered deposit A:  $depositAEntrances
      |\tPeople who entered deposit B:  $depositBEntrances
      |\tPeople who signed deposit A:   $depositASignatures
      |\tPeople who signed deposit B:   $depositBSignatures
      |\tDid any player decide to stop? $finished
    """.stripMargin
}

trait Move extends (State => State) {
  val player: Player
  final def canPlay(state: State): Boolean =
    state.playerTurn == player && internalCanPlay(state) && !state.finished
  protected def internalCanPlay(state: State): Boolean
  override def toString(): String
}

object Move {
  def forPlayers[A](ctor: Player => A) = Player.values.map(ctor)
  val moves = TransferMoney :: List(EnterDepositA, EnterDepositB, SignDepositA, SignDepositB, StopPlaying).flatMap(forPlayers)
}

case class EnterDepositA(player: Player) extends Move {
  protected def internalCanPlay(state: State) = !state.depositAEntrances.contains(player)
  def apply(state: State) = {
    val newState = state.changeTurn.copy(depositAEntrances = state.depositAEntrances + player)
    if (newState.depositAEntrances == Player.values.toSet)
      newState.copy(payoff = newState.payoff.mapValues(_ - DepositA))
    else
      newState
  }
  override def toString() = s"Player $player enters deposit A"
}

case class EnterDepositB(player: Player) extends Move {
  protected def internalCanPlay(state: State) = !state.depositBEntrances.contains(player)
  def apply(state: State) = {
    val newState = state.changeTurn.copy(depositBEntrances = state.depositBEntrances + player)
    if (newState.depositBEntrances == Player.values.toSet)
      newState.copy(payoff = newState.payoff.mapValues(_ - DepositB))
    else
      newState
  }
  override def toString() = s"Player $player enters deposit B"
}

case class SignDepositA(player: Player) extends Move {
  protected def internalCanPlay(state: State) = state.depositAExists && !state.depositASignatures.contains(player)
  def apply(state: State) = state.changeTurn.copy(
    depositASignatures = state.depositASignatures + player,
    payoff = state.payoff.collect {
      case (p, v) if p != player && p == Sam => (p, v + DepositA - ContractAmount)
      case (p, v) if p != player && p == Bob => (p, v + DepositA + ContractAmount)
      case other => other
    })

  override def toString() = s"Player $player signs deposit A"
}

case class SignDepositB(player: Player) extends Move {
  protected def internalCanPlay(state: State) = state.depositBExists && !state.depositBSignatures.contains(player)
  def apply(state: State) = {
    val newState = state.changeTurn.copy(depositBSignatures = state.depositBSignatures + player)
    if (newState.depositBSignatures == Player.values)
      newState.copy(payoff = newState.payoff.mapValues(_ + DepositB))
    else
      newState
  }
  override def toString() = s"Player $player signs deposit B"
}

case class StopPlaying(player: Player) extends Move {
  protected def internalCanPlay(state: State) = true
  def apply(state: State) = state.changeTurn.copy(finished = true)
  override def toString = s"Player $player stops playing"
}

object TransferMoney extends Move {
  val player = Bob
  protected def internalCanPlay(state: State) = !state.paymentMade && state.depositAExists
  def apply(state: State) = state.changeTurn.copy(
    payoff = Map(
      Sam -> (state.payoff(Sam) + ContractAmount),
      Bob -> (state.payoff(Bob) - ContractAmount)),
    paymentMade = true)
  override def toString() = s"Bob transfers money to Sam"
}