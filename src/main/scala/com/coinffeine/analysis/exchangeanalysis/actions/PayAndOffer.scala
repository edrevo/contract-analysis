package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.State

/** This is a combined action of Pay and Offer */
case object PayAndOffer extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = {
    Pay.internalCanPlay(state) && Offer.internalCanPlay(Pay.play(state).changeTurn)
  }
  override def play(state: State): State = Offer.play(Pay.play(state).changeTurn)
}
