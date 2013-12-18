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
  override def toShortString = "enter-A"
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
  override def toShortString = "enter-B"
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
  override def toShortString = "sign-A"
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
  protected def internalCanPlay(state: State) = !state.paymentMade && state.depositAExists
  def apply(state: State) = state.changeTurn.copy(
    payoff = Map(
      Sam -> (state.payoff(Sam) + ContractAmount),
      Bob -> (state.payoff(Bob) - ContractAmount)),
    paymentMade = true)
  override def toString() = s"Bob transfers money to Sam"
  override def toShortString = "pay-€"
}
