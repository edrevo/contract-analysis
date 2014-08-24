package coinffeine.analysis.exchange

import coinffeine.analysis.{Payoffs, Payoff}

trait FeePolicy {
  def pay(fiat: Payoffs, amount: Payoff): Payoffs
}

/** @constructor
  * @param fee  Fee the payment processor charges, as a percentage
  */
class BuyerPaidFee(fee: Double) extends FeePolicy {
  override def pay(fiat: Payoffs, amount: Payoff): Payoffs = fiat + Payoffs(
      bob = -(1 + fee) * amount,
      sam = amount)
}

/** @constructor
  * @param fee  Fee the payment processor charges, as a percentage
  */
class SellerPaidFee(fee: Double) extends FeePolicy {
  override def pay(fiat: Payoffs, amount: Payoff): Payoffs = fiat + Payoffs(
    bob = amount,
    sam = (1 - fee) * amount)
}
