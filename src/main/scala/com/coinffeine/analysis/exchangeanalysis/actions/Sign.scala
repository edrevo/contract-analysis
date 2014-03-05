package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.State

/** Represents Sam signing Bob's latest offer */
case object Sign extends Action {
  override val player = Sam
  override private[actions] def internalCanPlay(state: State) =
    state.lastOffer.isDefined && state.lastOffer != state.lastSignedOffer
  override def play(state: State): State = state.signOffer.changeTurn
}
