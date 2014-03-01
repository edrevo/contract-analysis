package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.State

/** Represents Sam signing Bob's latest offer */
case object Sign extends Action {
  override val player = Sam
  override private[actions] def internalCanPlay(state: State) =
    state.lastOffer.isDefined && state.lastOffer != state.lastSignedOffer
  override def play(state: State): State = state.signOffer.changeTurn
}
