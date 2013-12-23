package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Constants {
  val N = 10
  val ContractAmount: Payoff = 100
  val ContractStep = ContractAmount / N
  val ChannelAmounts: Payoffs = Map(
    Bob -> ContractStep,
    Sam -> ContractAmount
  )
  val InitialValues: Payoffs = Map(
    Bob -> (ContractAmount + ChannelAmounts(Bob)),
    Sam -> ChannelAmounts(Sam)
  )
  val ConsumerSurplus = BigDecimal(1) / (N - 1)
}
