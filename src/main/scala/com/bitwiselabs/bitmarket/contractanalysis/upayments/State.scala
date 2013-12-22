package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._

case class State(
  nextPlayer: Player,
  uPaymentChannels: Set[Player] = Set.empty,
  amountPaid: Payoff = 0,
  lastOffer: Option[Payoffs] = None,
  lastSignedOffer: Option[Payoffs] = None,
  lastOfferPublished: Boolean = false,
  unresponsiveSam: Boolean = false,
  timedOut: Boolean = false
) {
  require(!(lastOfferPublished && timedOut))
  require(!lastOfferPublished || lastSignedOffer.isDefined)

  def uPaymentChannelsExist: Boolean = uPaymentChannels.size == 2

  def payoffs: Payoffs = {
    var p = Map(
      Bob -> (InitialValues(Bob) - amountPaid),
      Sam -> (InitialValues(Sam) + amountPaid * (1 + ConsumerSurplus))
    )
    if (uPaymentChannelsExist) {
      p = psub(p, ChannelAmounts)
    }
    if (lastOfferPublished) {
      p = psum(p, lastSignedOffer.get)
      p = p.updated(Bob, p(Bob) + lastSignedOffer.get.apply(Bob) * ConsumerSurplus)
    }
    if (timedOut) {
      p = psum(p, ChannelAmounts)
    }
    p
  }

  def changeTurn = this.copy(nextPlayer = nextPlayer match {
    case Bob => if (unresponsiveSam) Bob else Sam
    case Sam => Bob
  })

  def signOffer = this.copy(lastSignedOffer = lastOffer)

  def pay = this.copy(amountPaid = amountPaid + ContractStep)

  def enter(player: Player) = this.copy(uPaymentChannels = uPaymentChannels + player)
}

object State {
  def initialState(initialPlayer: Player) = State(initialPlayer)
}
