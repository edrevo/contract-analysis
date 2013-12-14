package com.bitwiselabs.bitmarket

import com.bitwiselabs.bitmarket.contractanalysis.Player._

package object contractanalysis {
  type MoveMap = Map[Move, State]

  type History = List[Move]
  val EmptyHistory: History = List.empty

  type Payoff = Map[Player, Int]
}
