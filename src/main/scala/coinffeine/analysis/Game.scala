package coinffeine.analysis

import scala.annotation.tailrec

abstract class Game[Action] {
  type State = GameState[Action]
  type Edges = Map[Action, State]
  val noChildren: Edges = Map.empty

  case class GameTree(tree: Map[State, Edges]) {
    val root = initialState
    type History = Seq[Action]
    val emptyHistory: History = List.empty

    lazy val stateSet: Set[State] = tree.keySet

    def addNonLeaf(state: State, edges: Edges) = GameTree(tree + (state -> edges))

    def addLeafStates(states: Seq[State]): GameTree = {
      val leaves = states.map { s => s -> noChildren }
      GameTree(tree ++ leaves)
    }

    def contains(state: State) = tree.contains(state)

    lazy val dominantStrategies: Set[History] = {
      var seen: Map[State, Map[History, Payoffs]] = Map.empty

      def resolve(state: State): Map[History, Payoffs] = {
        if (tree(state).isEmpty) Map(emptyHistory -> state.payoffs)
        else if (seen.contains(state)) seen(state)
        else {
          val subTreePayoffs: Seq[(Payoff, Action, Map[History, Payoffs])] = for {
            (action, childState) <- tree(state).toSeq
            bestStrategies = resolve(childState)
            minPayoff = bestStrategies.values.map(payoffs => payoffs(state.currentPlayer)).min
          } yield (minPayoff, action, bestStrategies)
          val bestMinPayoff = subTreePayoffs.maxBy(_._1)._1
          val result = (for {
            (`bestMinPayoff`, action, bestStrategies) <- subTreePayoffs
            (history, payoffs) <- bestStrategies
          } yield (action +: history) -> payoffs).toMap
          seen = seen.updated(state, result)
          result
        }
      }

      resolve(initialState).keySet
    }

    lazy val containsDesiredStrategy: Boolean = dominantStrategies.contains(desiredStrategy)
  }

  object GameTree {
    val empty = GameTree(Map.empty)
  }

  val initialState: GameState[Action]

  val desiredStrategy: Seq[Action]

  lazy val desiredStrategyStates: Set[GameState[Action]] =
    desiredStrategy.scanLeft(initialState)(_.play(_)).toSet

  def completeTree(maxDepth: Int = Integer.MAX_VALUE): GameTree = {

    def allEdgesByState(state: State): Edges =
      (for (action <- state.actions) yield action -> state.play(action)).toMap

    buildTree(allEdgesByState, maxDepth)
  }

  def plausibleTree(strategies: Set[AllowedActions[Action]],
                    maxDepth: Int = Integer.MAX_VALUE): GameTree = {

    val strategiesByPlayer: Map[Player, AllowedActions[Action]] =
      strategies.map { strat => strat.player -> strat }.toMap

    def plausibleEdgesByState(state: State): Edges = {
      val actions = strategiesByPlayer(state.currentPlayer).allowedActions(state)
      (for (action <- actions) yield action -> state.play(action)).toMap
    }

    buildTree(plausibleEdgesByState, maxDepth)
  }

  private def buildTree(edgesByState: State => Edges, maxDepth: Int): GameTree = {

    @tailrec
    def visit(unseen: Seq[State], seen: GameTree, maxDepth: Int): GameTree = {
      if (unseen.isEmpty) seen
      else if (maxDepth <= 0) seen.addLeafStates(unseen)
      else {
        val current = unseen.head
        if (seen.contains(current)) visit(unseen.tail, seen, maxDepth)
        else {
          val children: Edges = if (current.isFinal) Map.empty else edgesByState(current)
          val unseenChildren = children.values
          visit(unseen.tail ++ unseenChildren, seen.addNonLeaf(current, children), maxDepth - 1)
        }
      }
    }

    visit(Seq(initialState), GameTree.empty, maxDepth)
  }
}
