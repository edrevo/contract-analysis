/*
 * Telefónica Digital - Product Development and Innovation
 *
 * THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Copyright (c) Telefónica Investigación y Desarrollo S.A.U.
 * All rights reserved.
 */

package com.bitwiselabs.bitmarket.contractanalysis

object Graveyard {
/*  class TreeResolver(
                      tree: Map[State, Main.MoveMap],
                      initialState: State,
                      tentativeResolution: Map[State, Map[List[Move], Main.Payoff]] = Map()) {

    var memoize : Map[State, Map[State, Map[List[Move], Main.Payoff]]] = Map()

    def apply(state: State = initialState, seen: Set[State] = Set()): Map[State, Map[List[Move], Main.Payoff]] = {
      if (seen.contains(state)) {
        if (tentativeResolution.contains(state)) {
          return Map(state -> tentativeResolution(state))
        } else {
          return Map(state -> Map(List() -> state.payoff))
        }
      }
      if(memoize.contains(state)) {
        return memoize(state)
      }
      val resolution = for {
        (move, newState) <- tree(state)
        value <- apply(newState, seen + state)
      } yield value
      val payoffs = for {
        (move, newState) <- tree(state)
        (moves, payoff) <- resolution(newState)
      } yield (move :: moves) -> payoff
      val player = state.playerTurn
      val other = state.changeTurn.playerTurn
      val maxPlayer: Int = payoffs.maxBy(_._2(player))._2(player)
      val maxPlayerPayoffs = payoffs.filter(_._2(player) == maxPlayer)
      val minOtherPlayer = maxPlayerPayoffs.minBy(_._2(other))._2(other)
      val resultingPayoffs = maxPlayerPayoffs.filter(_._2(other) == minOtherPlayer)
      memoize = memoize.updated(state, resolution + (state -> resultingPayoffs))
      memoize(state)
    }
  }

  case class State(
                    playerTurn: Value,
                    payoff: Map[Player, Int] = Map(Sam -> ValueSam, Bob -> ValueBob),
                    paymentMade: Boolean = false,
                    depositAEntrances: Set[Player] = Set(),
                    depositBEntrances: Set[Player] = Set(),
                    depositASignatures: Set[Player] = Set(),
                    depositBSignatures: Set[Player] = Set()) {
    val depositAExists = depositAEntrances == Player.values.toSet
    val depositBExists = depositBEntrances == Player.values.toSet
    def changeTurn = copy(playerTurn = playerTurn match {
      case Sam => Bob
      case Bob => Sam
    })

    if (depositASignatures.nonEmpty)
      require(depositAExists, this)
    if (depositBSignatures.nonEmpty)
      require(depositBExists, this)
    require(payoff.values.forall(_ >= 0), this)
  }

  trait Move extends (State => State) {
    val player: Player
    final def canPlay(state: State): Boolean = state.playerTurn == player && internalCanPlay(state)
    protected def internalCanPlay(state: State): Boolean
    override def toString: String
  }

  object Move {
    def forPlayers[A](ctor: Player => A) = Player.values.map(ctor)
    val moves = TransferMoney :: List(EnterDepositA, EnterDepositB, SignDepositA, SignDepositB, Skip).flatMap(forPlayers)
  }

  case class EnterDepositA(player: Player) extends Move {
    protected def internalCanPlay(state: State) = !state.depositAEntrances.contains(player)
    def apply(state: State) = {
      val newState = state.changeTurn.copy(depositAEntrances = state.depositAEntrances + player)
      if (newState.depositAEntrances == Player.values.toSet)
        newState.copy(payoff = newState.payoff.mapValues(_ - DepositA))
      else
        newState
    }
    override def toString = s"Player $player enters deposit A"
  }

  case class EnterDepositB(player: Player) extends Move {
    protected def internalCanPlay(state: State) = !state.depositBEntrances.contains(player)
    def apply(state: State) = {
      val newState = state.changeTurn.copy(depositBEntrances = state.depositBEntrances + player)
      if (newState.depositBEntrances == Player.values.toSet)
        newState.copy(payoff = newState.payoff.mapValues(_ - DepositB))
      else
        newState
    }
    override def toString = s"Player $player enters deposit B"
  }

  case class SignDepositA(player: Player) extends Move {
    protected def internalCanPlay(state: State) = state.depositAExists && !state.depositASignatures.contains(player)
    def apply(state: State) = state.changeTurn.copy(
      depositASignatures = state.depositASignatures + player,
      payoff = state.payoff.collect {
        case (p, v) if p != player && p == Sam => (p, v + DepositA - ContractAmount)
        case (p, v) if p != player && p == Bob => (p, v + DepositA + ContractAmount)
        case other => other
      })

    override def toString = s"Player $player signs deposit A"
  }

  case class SignDepositB(player: Player) extends Move {
    protected def internalCanPlay(state: State) = state.depositBExists && !state.depositBSignatures.contains(player)
    def apply(state: State) = {
      val newState = state.changeTurn.copy(depositBSignatures = state.depositBSignatures + player)
      if (newState.depositBSignatures == Player.values)
        newState.copy(payoff = newState.payoff.mapValues(_ + DepositB))
      else
        newState
    }
    override def toString = s"Player $player signs deposit B"
  }

  case class Skip(player: Player) extends Move {
    protected def internalCanPlay(state: State) = true
    def apply(state: State) = state.changeTurn
    override def toString = s"Player $player skips turn"
  }

  object TransferMoney extends Move {
    val player = Bob
    protected def internalCanPlay(state: State) = !state.paymentMade
    def apply(state: State) = state.changeTurn.copy(
      payoff = Map(
        Sam -> (state.payoff(Sam) + ContractAmount),
        Bob -> (state.payoff(Bob) - ContractAmount)),
      paymentMade = true)
    override def toString = s"Bob transfers money to Sam"
  } */
}
