package com.bitwiselabs.bitmarket.contractanalysis

import com.bitwiselabs.bitmarket.contractanalysis.Player._

package object upayments {
  type Payoff = BigDecimal
  type Payoffs = Map[Player, Payoff]

  def lift(op: (Payoff, Payoff) => Payoff)(left: Payoffs, right: Payoffs): Payoffs = for {
    (player, value) <- left
  } yield (player, op(value, right(player)))

  val psum = lift(_ + _) _
  val psub = lift(_ - _) _
}
