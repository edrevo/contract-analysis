package com.coinffeine.analysis.exchange

sealed trait ExchangeAction

object ExchangeAction {
  val all: Set[ExchangeAction] = Set(Enter, Pay, Publish, Sign, Wait)
}

case object Leave extends ExchangeAction
case object Enter extends ExchangeAction
case object Pay extends ExchangeAction
case object Publish extends ExchangeAction
case object Sign extends ExchangeAction
case object Wait extends ExchangeAction


