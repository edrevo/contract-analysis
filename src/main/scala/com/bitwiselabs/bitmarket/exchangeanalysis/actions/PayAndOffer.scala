package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.State

/** This is a combined action of Pay and Offer */
case object PayAndOffer extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = {
    Pay.internalCanPlay(state) && Offer.internalCanPlay(Pay.play(state).changeTurn)
  }
  override def play(state: State): State = Offer.play(Pay.play(state).changeTurn)
}
