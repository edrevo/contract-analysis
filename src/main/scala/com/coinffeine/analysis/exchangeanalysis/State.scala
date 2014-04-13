package com.coinffeine.analysis.exchangeanalysis

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.Constants._
import com.coinffeine.analysis.exchangeanalysis.actions.Action

/**
  * Represents the full state of a game's node
  * @constructor
  * @param nextPlayer The player that will play the next action
  * @param playersInUPaymentChannels The set of players that have entered the micro-payment channel
  * @param amountPaid How much fiat money has Bob paid to Sam
  * @param lastOffer The last transaction Bob offered Sam
  * @param lastSignedOffer The last of Bob's offers that Sam signed
  * @param lastOfferPublished The transaction that Bob decided to publish
  * @param unresponsiveSam Whether Sam has decided to abandon the game
  * @param timedOut Whether the time lock for the micro-payment channel has expired and the refund
  *                 transactions have been redeemed
  */
case class State(
    nextPlayer: Player,
    playersInUPaymentChannels: Set[Player] = Set.empty,
    amountPaid: Payoff = 0,
    lastOffer: Option[Payoffs] = None,
    lastSignedOffer: Option[Payoffs] = None,
    lastOfferPublished: Boolean = false,
    unresponsiveSam: Boolean = false,
    timedOut: Boolean = false) {
  require(!(lastOfferPublished && timedOut))
  require(!lastOfferPublished || lastSignedOffer.isDefined)

  /** True if the actual exchange can begin */
  def uPaymentChannelsExist: Boolean = playersInUPaymentChannels.size == 2

  def payoffs: Payoffs = {
    var payoffs = Payoffs(
      bob = InitialValues(Bob) - amountPaid,
      sam = InitialValues(Sam) + amountPaid * (1 + ConsumerSurplus)
    )
    if (uPaymentChannelsExist) {
      payoffs = payoffs - ChannelAmounts
    }
    if (lastOfferPublished) {
      payoffs = payoffs + lastSignedOffer.get
      payoffs = payoffs.copy(bob = payoffs(Bob) + lastSignedOffer.get.apply(Bob) * ConsumerSurplus)
    }
    if (timedOut) {
      payoffs = payoffs + ChannelAmounts
    }
    payoffs
  }

  /** The player that cannot play the next action */
  lazy val otherPlayer = nextPlayer match {
    case Bob => Sam
    case Sam => Bob
  }

  def changeTurn = this.copy(nextPlayer = nextPlayer match {
    case Bob => if (unresponsiveSam) Bob else Sam
    case Sam => Bob
  })

  def signOffer = this.copy(lastSignedOffer = lastOffer)

  def pay = this.copy(amountPaid = amountPaid + ContractStep)

  def enter(player: Player) = this.copy(playersInUPaymentChannels = playersInUPaymentChannels + player)
}

object State {
  def initialState(initialPlayer: Player) = State(initialPlayer)

  /** The sequence of states the will take place if the exchange happens */
  lazy val happyStates: List[State] = Action.happyPath.scanLeft(Action.initialState)(
    (state, action) => action.play(state))
}
