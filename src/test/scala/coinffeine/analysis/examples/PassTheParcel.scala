package coinffeine.analysis.examples

import coinffeine.analysis._
import coinffeine.analysis.graphviz.GameTreeVisualization

object PassTheParcel {

  sealed trait Action
  case object Pass extends Action
  case object Stop extends Action

  case class State(round: Int, maxRounds: Int, stopped: Boolean) extends GameState[Action] {

    override val currentPlayer: Player = round % 2 match {
      case 0 => Sam
      case 1 => Bob
    }

    private def lastPlayer = currentPlayer.otherPlayer

    override def actions: Set[Action] =
      if (round < maxRounds && !stopped) Set(Pass, Stop)
      else Set.empty

    override def play(action: Action) = {
      require(!isFinal)
      action match {
        case Pass => copy(round = round + 1)
        case Stop => copy(round = round + 1, stopped = true)
      }
    }

    override def payoffs = Payoffs(
      sam = 2 * passes + (if (hasStopped(Sam)) 3 else 0),
      bob = 2 * passes + (if (hasStopped(Bob)) 3 else 0)
    )

    private def passes: Int = if (stopped) round - 1 else round

    private def hasStopped(player: Player) = stopped && lastPlayer == player
  }

  case class CollaborativeStrategy(override val player: Player) extends PureStrategy[Action] {
    override def selectAction(state: GameState[Action]): Action = Pass
  }

  val maxRounds = 10
  val initialState = State(0, maxRounds, stopped = false)
  val happyPath = List.fill(maxRounds)(Pass)
  val game = Game[Action](initialState)

  def analyzeCompleteGame = game.completeTree()
  def analyzeGame(collaborativePlayers: Set[Player], randomPlayers: Set[Player]) = {
    val collabStrats = collaborativePlayers.map { player => CollaborativeStrategy(player).allowedActions }
    val randomStrats = randomPlayers.map { player => AllowedActions.any[Action](player) }
    game.plausibleTree(collabStrats ++ randomStrats)
  }

  def main(args: Array[String]): Unit = {
    val tree = analyzeGame(collaborativePlayers = Set(Bob), randomPlayers = Set(Sam))

    println("Game tree:")
    println(tree)
    println("Dominant strategies")
    println(tree.dominantStrategies)
    new GameTreeVisualization[Action, game.type](tree, happyPath, tree.dominantStrategies)
      .writeTo("pass-the-parcel.dot")
  }
}
