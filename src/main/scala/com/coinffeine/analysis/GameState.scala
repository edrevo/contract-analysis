package com.coinffeine.analysis

trait GameState[Action] {

  val currentPlayer: Player
  def payoffs: Payoffs
  def actions: Set[Action]
  def play(action: Action): GameState[Action]

  def isFinal: Boolean = actions.isEmpty
  def playAll(actions: Seq[Action]): GameState[Action] = actions.foldLeft(this)(_.play(_))
  def historyOf(actions: Seq[Action]): Seq[GameState[Action]] = actions.scanLeft(this)(_.play(_))
}
