package coinffeine.analysis.exchange

import coinffeine.analysis.{Payoff, Payoffs}

/** Parameters of an exchange game.
  *
  * @constructor
  * @param steps             The number of steps in which the exchange will happen
  * @param contractAmount    The principal we want to exchange
  * @param consumerSurplus   The percentage of how much each player values the currency they don't
  *                          own. For example, a value of 0.1 means that Bob values 1 BTC like 1.1
  *                          fiat units and Sam values 1 fiat unit like 1.1 BTC
  * @param feePolicy           Fee the payment processor charges, as a percentage
  * @param lastStepFactor    The difference (as a %) between a normal step and the last step. If
  *                          this param is 1 then the last step has the same value as the rest of
  *                          the steps. If its value is lower than one then the last step is
  *                          smaller. If its value is greater than one, then the last step is
  *                          greater than the normal steps.
  */
case class Parameters(
  steps: Int = 10,
  contractAmount: Payoff = 100,
  consumerSurplus: BigDecimal = 0.06,
  feePolicy: FeePolicy = new SellerPaidFee(0.05),
  lastStepFactor: BigDecimal = 0.75) {

  require(steps > 0)
  require(contractAmount > 0)

  /** The amount of value that will be exchanged on each step */
  val contractStep: Payoff = contractAmount / (steps - 1 + lastStepFactor)

  val lastContractStep: Payoff = lastStepFactor * contractStep

  /** The amount of fiat each player needs in order to enter the exchange. We assume
    * 1 fiat unit = 1 BTC, since the specific exchange rate doesn't affect the mechanics
    * of the game.
    */
  val fiatAmounts: Payoffs = Payoffs(bob = contractAmount, sam = 0)

  /** The deposit amount each players needs for the exchange */
  val depositAmounts: Payoffs = Payoffs(
    bob = 2 * contractStep,
    sam = contractStep)

  /** The amount of bitcoins that will be committed into the micropayment channel */
  val btcAmounts: Payoffs = depositAmounts + Payoffs(
    bob = 0,
    sam = contractAmount
  )

  /** The total amount of value each player needs in order to enter the exchange */
  val initialBalances: Balances = Balances(btcAmounts, fiatAmounts, feePolicy)
  val initialValues: Payoffs = initialBalances.utilities(consumerSurplus)

  val refundPenalization: Payoffs = Payoffs(contractStep, contractStep)

  val firstOffer = Payoffs(bob = 0, sam = contractAmount)
  val exchangeDelta = Payoffs(
    bob = contractStep,
    sam = -contractStep

  )
  /** The special, final offer Bob will offer Sam which includes getting back the deposits */
  val finalOffer = Payoffs(
    bob = contractAmount + 2 * contractStep,
    sam = contractStep
  )
  /** The sequence of micro payment channel transactions */
  val offers: Vector[Payoffs] =
    Vector.fill(steps - 1)(exchangeDelta).scanLeft(firstOffer)(_ + _) :+ finalOffer
}

object Parameters {
  val default = Parameters()
}
