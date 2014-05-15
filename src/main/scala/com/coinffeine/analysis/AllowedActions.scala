package com.coinffeine.analysis

trait AllowedActions[Action] {
  val player: Player
  def allowedActions(state: GameState[Action]): Set[Action]
}

object AllowedActions {
  def any[Action](p: Player): AllowedActions[Action] = new AllowedActions[Action] {
    override val player: Player = p
    override def allowedActions(state: GameState[Action]) = state.actions
  }
}
