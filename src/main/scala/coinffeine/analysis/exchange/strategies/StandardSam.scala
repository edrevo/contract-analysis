package coinffeine.analysis.exchange.strategies

import coinffeine.analysis._
import coinffeine.analysis.exchange._
import coinffeine.analysis.exchange.ExchangingState
import coinffeine.analysis.exchange.HandshakeState

object StandardSam extends PureStrategy[ExchangeAction] {

  override val player = Sam

  override def selectAction(state: GameState[ExchangeAction]) = state match {
    case HandshakeState(_, _) => Enter
    case exState: ExchangingState =>
      if (exState.stepsPaid >= exState.txSigned) Sign else Wait
    case _ => throw new IllegalArgumentException("No standard move to play")
  }
}
