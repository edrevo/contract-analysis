package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Constants._
import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.{Payoffs, State}

/** Represents Bob providing a new offer to Sam */
case object Offer extends Action {
  private val defaultFirstOffer: Payoffs = Payoffs(
    bob = 0,
    sam = ContractAmount)
  private val exchangeDelta = Payoffs(
    bob = ContractStep,
    sam = -ContractStep)
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = {
    val offer = nextOffer(state)
    val nonNegativeAmounts = offer.forall(_ >= 0)
    val feasibleTotalAmount = offer.sum <= ChannelAmounts.sum
    state.uPaymentChannelsExist && !state.unresponsiveSam &&
      nonNegativeAmounts && feasibleTotalAmount && state.lastOffer != Some(FinalOffer)
  }
  override def play(state: State): State = {
    val newOffer = nextOffer(state)
    val newState = state.copy(lastOffer = Some(newOffer)).changeTurn
    newState
  }
  private def nextOffer(state: State) = {
    val newOffer = state.lastOffer.getOrElse(defaultFirstOffer) + exchangeDelta
    if(newOffer(Sam) < 0) FinalOffer
    else newOffer
  }
}
