package com.coinffeine.analysis

case class Payoffs(bob: Payoff, sam: Payoff) extends Seq[Payoff] {

  def apply(player: Player) = player match {
    case Bob => bob
    case Sam => sam
  }

  def +(rhs: Payoffs) = Payoffs(
    bob + rhs.bob,
    sam + rhs.sam
  )

  def -(rhs: Payoffs) = Payoffs(
    bob - rhs.bob,
    sam - rhs.sam)

  def scaleBy(bobFactor: BigDecimal = 1, samFactor: BigDecimal = 1): Payoffs =
    Payoffs(bob * bobFactor, sam * samFactor)

  private val seq_ = Seq(bob, sam)

  override def apply(idx: Int) = seq_(idx)

  override def iterator = seq_.iterator

  override def length = 2
}

object Payoffs {

  val zero = Payoffs(0, 0)

  def apply(pairs: (Player, Payoff)*): Payoffs = {
    require(pairs.size == 2)
    val map = pairs.toMap
    require(map.size == 2)
    Payoffs(bob = map(Bob), sam = map(Sam))
  }
}
