package com.coinffeine.analysis.exchange.strategies

import com.coinffeine.analysis._
import com.coinffeine.analysis.exchange._
import com.coinffeine.analysis.exchange.ExchangingState
import com.coinffeine.analysis.exchange.HandshakeState

object StandardSam extends PureStrategy[ExchangeAction] {

  override val player = Sam

  override def selectAction(state: GameState[ExchangeAction]) = state match {
    case HandshakeState(_, _) => Enter
    case exState: ExchangingState =>
      if (exState.stepsPaid >= exState.txSigned) Sign else Wait
    case _ => throw new IllegalArgumentException("No standard move to play")
  }
}
