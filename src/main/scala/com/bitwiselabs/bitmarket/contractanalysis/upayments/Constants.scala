package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Constants {
  val N = 10
  val CorruptionRatio = BigDecimal(0)
  val ContractAmount: Payoff = 100
  val ContractStep = ContractAmount / N
  val ChannelAmounts: Payoffs = Map(
    Bob -> (2 * ContractStep),
    Sam -> (ContractAmount + ContractStep)
  )
  val InitialValues: Payoffs = Map(
    Bob -> (ChannelAmounts(Bob) + ContractAmount),
    Sam -> ChannelAmounts(Sam)
  )
  val FinalOffer: Payoffs = Map(Sam -> ContractStep, Bob -> (ContractAmount + 2 * ContractStep))
  val ConsumerSurplus = BigDecimal(0)
}
