package com.coinffeine.analysis.exchangeanalysis.actions

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.{State, Constants}
import Constants._

/** This action represents Bob paying an exchange's step worth of fiat money to Sam */
case object Pay extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = state.uPaymentChannelsExist &&
    state.amountPaid < ContractAmount && !state.unresponsiveSam
  override def play(state: State): State = state.pay.changeTurn
}
