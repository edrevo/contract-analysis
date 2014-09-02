package coinffeine.analysis.exchange

import coinffeine.analysis.Game

class ExchangeGame(parameters: Parameters)
  extends Game[ExchangeAction]() {

  lazy val desiredStrategy: Seq[ExchangeAction] =
    Seq(Enter, Enter, Sign) ++
    Seq.fill(parameters.steps)(Seq(Pay, Sign)).flatten ++
    List(Publish)

  lazy val initialState = ExchangeState.initial(parameters)
}
