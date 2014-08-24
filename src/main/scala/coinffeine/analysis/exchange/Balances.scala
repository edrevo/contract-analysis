package coinffeine.analysis.exchange

import coinffeine.analysis.{Payoff, Payoffs}

case class Balances(btc: Payoffs, fiat: Payoffs, feePolicy: FeePolicy) {

  /** Computes the utility for the players given a consumer surplus and assuming linearity. */
  def utilities(consumerSurplus: BigDecimal): Payoffs = {
    val factor = consumerSurplus + 1
    btc.scaleBy(bobFactor = factor) + fiat.scaleBy(samFactor = factor)
  }

  def pay(amount: Payoff): Balances = copy(fiat = feePolicy.pay(fiat, amount))

  def loseBtc(amounts: Payoffs): Balances = copy(btc = btc - amounts)
}
