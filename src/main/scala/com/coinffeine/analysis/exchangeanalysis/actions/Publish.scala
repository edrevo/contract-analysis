package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.State

case object Publish extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = state.lastSignedOffer.isDefined
  override def play(state: State): State = state.copy(
    lastOfferPublished = true,
    unresponsiveSam = false).changeTurn
}
