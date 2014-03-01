package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.{State, Constants}
import Constants._

/** This action represents Bob paying an exchange's step worth of fiat money to Sam */
case object Pay extends Action {
  override val player = Bob
  override private[actions] def internalCanPlay(state: State) = state.uPaymentChannelsExist &&
    state.amountPaid < ContractAmount && !state.unresponsiveSam
  override def play(state: State): State = state.pay.changeTurn
}
