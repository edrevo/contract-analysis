package com.bitwiselabs.bitmarket.exchangeanalysis

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._

case class Payoffs(val bob: Payoff, val sam: Payoff) extends Seq[Payoff] {
  def apply(player: Player) = player match {
    case Bob => bob
    case Sam => sam
  }

  def +(rhs: Payoffs) = Payoffs(
    bob + rhs.bob,
    sam + rhs.sam)

  def -(rhs: Payoffs) = Payoffs(
    bob - rhs.bob,
    sam - rhs.sam)

  private val seq_ = Seq(bob, sam)

  override def apply(idx: Int) = seq_(idx)

  override def iterator = seq_.iterator

  override def length = 2
}
