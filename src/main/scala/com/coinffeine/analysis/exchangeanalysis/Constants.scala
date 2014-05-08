package com.coinffeine.analysis.exchangeanalysis

object Constants {
  /** The number of steps in which the exchange will happen */
  val Steps = 10
  /** The principal we want to exchange */
  val ContractAmount: Payoff = 100
  /** The amount of value that will be exchanged on each step */
  val ContractStep = ContractAmount / Steps
  /** The deposit amount each players needs for the exchange */
  val DepositAmounts: Payoffs = Payoffs(
    bob = 2 * ContractStep,
    sam = ContractStep)
  /** The amount of bitcoins that will be committed into the micropayment channel */
  val ChannelAmounts: Payoffs = DepositAmounts + Payoffs(
    bob = 0,
    sam = ContractAmount)
  /** The amount of fiat each player needs in order to enter the exchange. We assume
    * 1 fiat unit = 1 BTC, since the specific exchange rate doesn't affect the mechanics
    * of the game.
    */
  val FiatAmounts: Payoffs = Payoffs(
   bob = ContractAmount,
   sam = 0)
  /** The total amount of value each player needs in order to enter the exchange */
  val InitialValues: Payoffs = ChannelAmounts + FiatAmounts
  /** The special, final offer Bob will offer Sam which includes getting back the deposits */
  val FinalOffer: Payoffs = Payoffs(
    bob = ContractAmount + 2 * ContractStep,
    sam = ContractStep)
  /** The percentage of how much each player values the currency they don't own.
    * For example, a value of 0.1 means that Bob values 1 BTC like 1.1 fiat units and
    * Sam values 1 fiat unit like 1.1 BTC
    */
  val ConsumerSurplus = BigDecimal(0)
}
