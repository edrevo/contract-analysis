package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.State

/** This is a "skip" move. Whoever plays it does nothing for the turn */
case class Wait(player: Player) extends Action {
  override private[actions] def internalCanPlay(state: State) = state.uPaymentChannelsExist
  def play(state: State): State = player match {
    case Bob => state.copy(timedOut = true).changeTurn
    case Sam => state.copy(unresponsiveSam = true).changeTurn
  }
}
