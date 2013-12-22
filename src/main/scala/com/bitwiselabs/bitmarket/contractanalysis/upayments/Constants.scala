package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Constants {
  val N = 10
  val ContractAmount: Payoff = 100
  val ChannelAmounts: Payoffs = Map(
    Bob -> ContractAmount / N,
    Sam -> ContractAmount
  )
  val ContractStep = ContractAmount / N
  val InitialValues: Payoffs = Map(
    Bob -> (ContractAmount + ChannelAmounts(Bob)),
    Sam -> ChannelAmounts(Sam)
  )
  val ConsumerSurplus = BigDecimal.valueOf(0) / 100
}
