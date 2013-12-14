package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._

object Main {
  type MoveMap = Map[Move, State]
  type History = List[Move]
  type Payoff = Map[Player, Int]

  val optimalSeq = Seq(
    EnterDepositB(Sam),
    EnterDepositB(Bob),
    EnterDepositA(Sam),
    EnterDepositA(Bob),
    Skip(Sam),
    TransferMoney,
    SignDepositA(Sam),
    SignDepositA(Bob),
    SignDepositB(Sam),
    SignDepositB(Bob))

  val states: Stream[State] = State(Sam) #:: optimalSeq.view.zip(states).map(p => p._1(p._2)).toStream

  def main(args: Array[String]) {
    println("Generating tree")
    val gameTree = generateGameTree(State(Sam))
    println("Done")
    println("Resolving tree")
    println(resolveTree(gameTree,
      State(
        Bob,
        payoff = Map(Bob -> ValueBob, Sam -> (ValueSam - DepositA + ContractAmount)),
        paymentMade = true,
        depositAEntrances = Player.values,
        depositBEntrances = Player.values,
        depositASignatures = Set(Sam),
        depositBSignatures = Player.values)))
  }

  def generateGameTree(initialState: State): Map[State, MoveMap] = {
    var result: Map[State, MoveMap] = Map()
    var unprocessedStates: Set[State] = Set(initialState)
    while (unprocessedStates.nonEmpty) {
      val state = unprocessedStates.head
      val moveMap = Move.moves.filter(_.canPlay(state)).map(move => (move, move(state))).toMap
      val unseenStates = moveMap.values.filterNot(result.keySet.contains).toSet
      unprocessedStates = (unprocessedStates ++ unseenStates) - state
      result = result.updated(state, moveMap)
    }
    result
  }

  def resolveTree(tree: Map[State, MoveMap], state: State, seen: Set[State] = Set()): Map[History, Payoff] = {
    val result: Map[History, Payoff] = if(tree(state).isEmpty) {
      Map(List() -> state.payoff)
    } else if (seen.contains(state)) {
      Map(List() -> state.payoff)
    } else {
      val subGamePayoffs = for {
        (move, newState) <- tree(state)
        subGameResolution = resolveTree(tree, newState, seen + state)
      } yield subGameResolution.values.map(payoff => payoff(state.playerTurn)).min -> (move, subGameResolution)
      val maxPlayerPayoff = subGamePayoffs.maxBy(_._1)._1
      for {
        (`maxPlayerPayoff`, pair) <- subGamePayoffs
        (move, resolution) = pair
        (history, payoff) <- resolution
      } yield (move :: history) -> payoff
    }
    println("---------------")
    println(state)
    if (!seen.contains(state) && !tree(state).isEmpty)
      for ((move, newState) <- tree(state)) {
        println(">>>>>" + resolveTree(tree, newState, seen + state).toString())
      }
    println(s"Options: \n\t${tree(state).keySet.mkString("\n\t")}")
    println(s"Result: ${result.mkString("\n\t", "\n\t", "")}")
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
}

trait Move extends (State => State) {
  val player: Player
  final def canPlay(state: State): Boolean = state.playerTurn == player && internalCanPlay(state) && !state.finished
  protected def internalCanPlay(state: State): Boolean
  override def toString: String
}

object Move {
  def forPlayers[A](ctor: Player => A) = Player.values.map(ctor)
  val moves = TransferMoney :: List(EnterDepositA, EnterDepositB, SignDepositA, SignDepositB, Skip, StopPlaying).flatMap(forPlayers)
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
  override def toString = s"Player $player enters deposit A"
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
  override def toString = s"Player $player enters deposit B"
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

  override def toString = s"Player $player signs deposit A"
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
  override def toString = s"Player $player signs deposit B"
}

case class Skip(player: Player) extends Move {
  protected def internalCanPlay(state: State) = true
  def apply(state: State) = state.changeTurn
  override def toString = s"Player $player skips turn"
}

case class StopPlaying(player: Player) extends Move {
  protected def internalCanPlay(state: State) = true
  def apply(state: State) = state.changeTurn.copy(finished = true)
  override def toString = s"Player $player stops playing"
}

object TransferMoney extends Move {
  val player = Bob
  protected def internalCanPlay(state: State) = !state.paymentMade
  def apply(state: State) = state.changeTurn.copy(
    payoff = Map(
      Sam -> (state.payoff(Sam) + ContractAmount),
      Bob -> (state.payoff(Bob) - ContractAmount)),
    paymentMade = true)
  override def toString = s"Bob transfers money to Sam"
}