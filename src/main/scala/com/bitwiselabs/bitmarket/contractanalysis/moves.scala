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
}

object Move {
  def forPlayers[A](ctor: Player => A) = Player.values.map(ctor)
  val moves = TransferMoney :: List(EnterDeposit, SignDepositA, SignDepositB, StopPlaying).flatMap(forPlayers)
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
      case (p, v) if p != player && p == Bob => (p, v + DepositA + ContractAmount)
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
      Sam -> (state.payoff(Sam) + ContractAmount),
      Bob -> (state.payoff(Bob) - ContractAmount)),
    paymentMade = true)
  override def toString() = s"Bob transfers money to Sam"
  override def toShortString = "pay-€"
}
