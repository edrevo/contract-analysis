package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import com.bitwiselabs.bitmarket.contractanalysis.upayments.Constants._

object Action {
  val all: Seq[Action] = Seq(
    Enter(Sam),
    Enter(Bob),
    Wait(Sam),
    Wait(Bob),
    Offer,
    Sign,
    Pay,
    PayAndOffer,
    Publish
  )
  val initialState = State.initialState(Bob)
  val happyPath: List[Action] =
    List(Enter(Bob), Enter(Sam), Offer, Sign) ++ List.fill(N)(List(PayAndOffer, Sign)).flatten ++
      List(Publish)
  val happyStates: List[State] = Action.happyPath.scanLeft(initialState)((state, action) => action.play(state))
  val isStandard = Map(Sam -> false, Bob -> true)
}

sealed trait Action {
  import Action._
  val player: Player
  final def canPlay(state: State): Boolean = {
    val stateIndex = happyStates.indexOf(state)
    val isValidIfStandard = !isStandard(player) || (stateIndex != -1 && this == happyPath.drop(stateIndex).headOption.getOrElse(null))
    (state.nextPlayer == player) && !(state.nextPlayer == Sam && state.unresponsiveSam) &&
      !state.timedOut && !state.lastOfferPublished && isValidIfStandard && internalCanPlay(state)
  }
  def internalCanPlay(state: State): Boolean
  def play(state: State): State
}

case class Enter(player: Player) extends Action {
  override def internalCanPlay(state: State) = !state.uPaymentChannels.contains(player)
  def play(state: State): State = state.enter(player).changeTurn
}

case class Wait(player: Player) extends Action {
  override def internalCanPlay(state: State) = state.uPaymentChannelsExist
  def play(state: State): State = player match {
    case Bob => state.copy(timedOut = true).changeTurn
    case Sam => state.copy(unresponsiveSam = true).changeTurn
  }
}

case object Publish extends Action {
  override val player = Bob
  override def internalCanPlay(state: State) = state.lastSignedOffer.isDefined
  override def play(state: State): State = state.copy(lastOfferPublished = true, unresponsiveSam = false).changeTurn
}

case object Offer extends Action {
  val defaultLastOffer: Payoffs = Map(Bob -> 0, Sam -> ContractAmount)
  override val player = Bob
  override def internalCanPlay(state: State) = {
    val offer = nextOffer(state)
    val nonNegativeAmounts = offer.values.forall(_ >= 0)
    val feasibleTotalAmount = offer.values.sum <= ChannelAmounts.values.sum
    state.uPaymentChannelsExist && !state.unresponsiveSam &&
      nonNegativeAmounts && feasibleTotalAmount && state.lastOffer != Some(FinalOffer)
  }
  override def play(state: State): State = {
    val newOffer = nextOffer(state)
    val newState = state.copy(lastOffer = Some(newOffer)).changeTurn
    newState
  }

  private def nextOffer(state: State) = {
    val newOffer = psum(state.lastOffer.getOrElse(defaultLastOffer), Map(Bob -> ContractStep, Sam -> (-ContractStep)))
    if(newOffer(Sam) < 0) FinalOffer
    else newOffer
  }
}

case object Sign extends Action {
  override val player = Sam
  override def internalCanPlay(state: State) = state.lastOffer.isDefined && state.lastOffer != state.lastSignedOffer
  override def play(state: State): State = state.signOffer.changeTurn
}

case object Pay extends Action {
  override val player = Bob
  override def internalCanPlay(state: State) = state.uPaymentChannelsExist &&
    state.amountPaid < ContractAmount && !state.unresponsiveSam
  override def play(state: State): State = state.pay.changeTurn
}

case object PayAndOffer extends Action {
  override val player = Bob
  override def internalCanPlay(state: State) = {
    Pay.internalCanPlay(state) && Offer.internalCanPlay(Pay.play(state).changeTurn)
  }
  override def play(state: State): State = Offer.play(Pay.play(state).changeTurn)
}
