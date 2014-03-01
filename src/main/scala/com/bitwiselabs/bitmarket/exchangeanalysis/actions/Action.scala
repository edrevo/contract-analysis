package com.bitwiselabs.bitmarket.exchangeanalysis.actions

import com.bitwiselabs.bitmarket.exchangeanalysis.Player._
import com.bitwiselabs.bitmarket.exchangeanalysis.{State, Constants}
import Constants._

/** A base trait for all actions */
trait Action {
  import Action._
  /** The player that can play this action */
  val player: Player

  /** Determines if the action can be played in a given state */
  final def canPlay(state: State): Boolean = {
    val isValidIfStandard =
      if (isStandard(player)) {
        val stateIndex = happyStates.indexOf(state)
        val stateIsPartOfHappyPath = stateIndex != -1
        val actionCreatesNextHappyState = this == happyPath.drop(stateIndex).headOption.orNull
        stateIsPartOfHappyPath && actionCreatesNextHappyState
      }
      else true
    val isPlayersTurn = state.nextPlayer == player
    val isUnresponsiveSamsTurn = state.nextPlayer == Sam && state.unresponsiveSam
    isPlayersTurn && !isUnresponsiveSamsTurn &&
      !state.timedOut && !state.lastOfferPublished && isValidIfStandard && internalCanPlay(state)
  }

  /** This method provides the logic that */
  private[actions] def internalCanPlay(state: State): Boolean

  /** Returns the resulting state of playing this action on the given state */
  def play(state: State): State
}

object Action {
  /** The list of all possible actions players can play */
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

  /** The initial state for the game */
  val initialState = State.initialState(Bob)

  /** The sequence of actions that need to take place for the exchange to happen */
  val happyPath: List[Action] =
    List(Enter(Bob), Enter(Sam), Offer, Sign) ++ List.fill(Steps)(List(PayAndOffer, Sign)).flatten ++
      List(Publish)

  /** A map that determines if a player is using the standard client or not.
    * Using the standard client restricts the player's moves. */
  val isStandard = Map(Sam -> false, Bob -> true)
}
