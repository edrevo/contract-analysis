package coinffeine.analysis.exchange.strategies

import coinffeine.analysis.{Bob, GameState, PureStrategy}
import coinffeine.analysis.exchange._

object StandardBob extends PureStrategy[ExchangeAction] {

  override val player = Bob

  override def selectAction(state: GameState[ExchangeAction]) = state match {

    case HandshakeState(_, _) => Enter

    case exState @ ExchangingState(_, stepsPaid, txSigned, unresponsiveSam) =>
      import exState.parameters._
      if (txSigned == steps + 1) Publish
      else if (txSigned > stepsPaid && stepsPaid < steps) Pay
      else if (unresponsiveSam && txSigned > 0) Publish
      else Wait

    case _ => throw new IllegalArgumentException("No standard move to play")
  }
}
