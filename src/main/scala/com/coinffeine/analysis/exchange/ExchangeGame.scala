package com.coinffeine.analysis.exchange

import com.coinffeine.analysis._
import com.coinffeine.analysis.Game

class ExchangeGame(parameters: Parameters)
  extends Game[ExchangeAction](ExchangeState.initial(parameters)) {

  lazy val happyPath: Seq[ExchangeAction] =
    Seq(Enter, Enter, Sign) ++
    Seq.fill(parameters.steps)(Seq(Pay, Sign)).flatten ++
    List(Publish)

  lazy val happyPathStates: Set[GameState[ExchangeAction]] =
    happyPath.scanLeft(initialState)(_.play(_)).toSet
}
