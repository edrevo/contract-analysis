package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.State

case object Publish extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = state.lastSignedOffer.isDefined
  override def play(state: State): State = state.copy(
    lastOfferPublished = true,
    unresponsiveSam = false).changeTurn
}
