package com.coinffeine.analysis.exchange

import com.coinffeine.analysis.{Payoff, Payoffs}

case class Balances(btc: Payoffs, fiat: Payoffs) {

  /** Computes the utility for the players given a consumer surplus and assuming linearity. */
  def utilities(consumerSurplus: BigDecimal): Payoffs = {
    val factor = consumerSurplus + 1
    btc.scaleBy(bobFactor = factor) + fiat.scaleBy(samFactor = factor)
  }

  def pay(amount: Payoff): Balances = copy(fiat = fiat + Payoffs(bob = -amount, sam = amount))

  def loseBtc(amounts: Payoffs): Balances = copy(btc = btc - amounts)
}
