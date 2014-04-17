package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Constants._
import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.{Payoffs, State}

/** Represents Sam providing a signature to Bob */
case object Sign extends Action {
  private val defaultFirstOffer: Payoffs = Payoffs(
    bob = 0,
    sam = ContractAmount)
  private val exchangeDelta = Payoffs(
    bob = ContractStep,
    sam = -ContractStep)
  override val player = Sam
  override private[actions] def internalCanPlay(state: State) = {
    val offer = nextOffer(state)
    val nonNegativeAmounts = offer.forall(_ >= 0)
    val feasibleTotalAmount = offer.sum <= ChannelAmounts.sum
    state.uPaymentChannelsExist && !state.unresponsiveSam &&
      nonNegativeAmounts && feasibleTotalAmount && state.lastSignedOffer != Some(FinalOffer)
  }

  override def play(state: State): State = state.signOffer(nextOffer(state)).changeTurn

  private def nextOffer(state: State) = {
    val newOffer = state.lastSignedOffer.getOrElse(defaultFirstOffer) + exchangeDelta
    if(newOffer(Sam) < 0) FinalOffer
    else newOffer
  }
}
