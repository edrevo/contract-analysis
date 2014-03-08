package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.State

/** This action represents a player committing funds to enable the actual exchange to happen */
case class Enter(player: Player) extends Action {
  override private[actions] def internalCanPlay(state: State) =
    !state.playersInUPaymentChannels.contains(player)
  def play(state: State): State = state.enter(player).changeTurn
}
