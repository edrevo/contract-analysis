package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Constants._
import com.bitwiselabs.bitmarket.contractanalysis.Player._

case class State(
    playerTurn: Value,
    payoff: Payoff = Map(Sam -> ValueSam, Bob -> ValueBob),
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

  override def toString =
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
