package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Constants._
import com.bitwiselabs.bitmarket.contractanalysis.Player._

sealed trait Move extends (State => State) {
  val player: Player
  final def canPlay(state: State): Boolean =
    state.playerTurn == player && internalCanPlay(state) && !state.finished
  protected def internalCanPlay(state: State): Boolean
  override def toString(): String
  def toShortString: String
  def tryPlay(state: State): Option[State] = if (canPlay(state)) Some(this(state)) else None
}

object Move {
  /** Pack a sequence of moves as a single one */
  def sequence(moves: Seq[Move]): Move = new Move {
    require(!moves.isEmpty)
    override def toShortString: String = moves.map(_.toShortString).mkString(", ")
    override def apply(initialState: State): State =
      moves.foldLeft(initialState)((state, move) => move(state))
    protected override def internalCanPlay(state: State): Boolean = {
      moves.foldLeft[Option[State]](Some(state)) { (maybeState, move) => move.tryPlay(state) }
    }.isDefined
    val player: Player.Player = moves.head.player
  }

  def forPlayers[A](ctor: Player => A) = Player.values.map(ctor).toList

  val moves = {
    val commonMoves = List(EnterDeposit, SignDepositB, StopPlaying).flatMap(forPlayers)
    val depositAMoves =
      if (hasBobDepositA) forPlayers(SignDepositA)
      else List(SignDepositA(Sam))
    TransferMoney :: (commonMoves ++ depositAMoves)
  }
}

case class EnterDeposit(player: Player) extends Move {
  protected def internalCanPlay(state: State) = !state.depositEntrances.contains(player)
  def apply(state: State) = {
    val newState = state.changeTurn.copy(depositEntrances = state.depositEntrances + player)
    if (newState.depositEntrances == Player.values.toSet)
      newState.copy(payoff = newState.payoff.mapValues(_ - DepositA - DepositB))
    else
      newState
  }
  override def toString() = s"Player $player enters deposits"
  override def toShortString = "enter"
}

case class SignDepositA(player: Player) extends Move {
  protected def internalCanPlay(state: State) = state.depositsExists && !state.depositASignatures.contains(player)
  def apply(state: State) = state.changeTurn.copy(
    depositASignatures = state.depositASignatures + player,
    payoff = state.payoff.collect {
      case (p, v) if p != player && p == Sam => (p, v + DepositA - ContractAmount)
      case (p, v) if p != player && p == Bob => (p, v + DepositA + ContractAmount + ConsumerSurplus)
      case other => other
    })

  override def toString() = s"Player $player signs deposit A"
  override def toShortString = "sign-A"
}

case class SignDepositB(player: Player) extends Move {
  protected def internalCanPlay(state: State) = state.depositsExists && !state.depositBSignatures.contains(player)
  def apply(state: State) = {
    val newState = state.changeTurn.copy(depositBSignatures = state.depositBSignatures + player)
    if (newState.depositBSignatures == Player.values)
      newState.copy(payoff = newState.payoff.mapValues(_ + DepositB))
    else
      newState
  }
  override def toString() = s"Player $player signs deposit B"
  override def toShortString = "sign-B"
}

case class StopPlaying(player: Player) extends Move {
  protected def internalCanPlay(state: State) = true
  def apply(state: State) = state.changeTurn.copy(finished = true)
  override def toString() = s"Player $player stops playing"
  override def toShortString = "stop"
}

object TransferMoney extends Move {
  val player = Bob
  protected def internalCanPlay(state: State) = !state.paymentMade && state.depositsExists
  def apply(state: State) = state.changeTurn.copy(
    payoff = Map(
      Sam -> (state.payoff(Sam) + ContractAmount + ConsumerSurplus),
      Bob -> (state.payoff(Bob) - ContractAmount)),
    paymentMade = true)
  override def toString() = s"Bob transfers money to Sam"
  override def toShortString = "pay-€"
}
