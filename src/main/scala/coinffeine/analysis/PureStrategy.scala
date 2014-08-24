package coinffeine.analysis

trait PureStrategy[Action] {

  val player: Player

  def selectAction(state: GameState[Action]): Action

  /** Returns the mixed strategy in which this pure strategy is chosen with probability 1 */
  lazy val allowedActions: AllowedActions[Action] = new AllowedActions[Action] {

    override val player: Player = PureStrategy.this.player

    override def allowedActions(state: GameState[Action]): Set[Action] =
      if (state.isFinal) Set.empty
      else Set(PureStrategy.this.selectAction(state))
  }
}
