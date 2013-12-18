package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Constants._
import com.bitwiselabs.bitmarket.contractanalysis.Player._

case class State(
    playerTurn: Value,
    payoff: Payoff = Map(Sam -> ValueSam, Bob -> ValueBob),
    paymentMade: Boolean = false,
    depositEntrances: Set[Player] = Set(),
    depositASignatures: Set[Player] = Set(),
    depositBSignatures: Set[Player] = Set(),
    finished: Boolean = false) {
  val depositsExists = depositEntrances == Player.values.toSet
  val otherPlayer = playerTurn match {
    case Sam => Bob
    case Bob => Sam
  }
  def changeTurn = copy(playerTurn = otherPlayer)

  if (depositASignatures.nonEmpty)
    require(depositsExists, this)
  if (depositBSignatures.nonEmpty)
    require(depositsExists, this)
  require(payoff.values.forall(_ >= 0), this)

  override def toString =
    s"""
      |\tTurn:                             $playerTurn
      |\tPayoff:                           $payoff
      |\tMoney transferred made:           $paymentMade
      |\tPeople who entered the deposits:  $depositEntrances
      |\tPeople who signed deposit A:      $depositASignatures
      |\tPeople who signed deposit B:      $depositBSignatures
      |\tDid any player decide to stop?    $finished
    """.stripMargin
}
