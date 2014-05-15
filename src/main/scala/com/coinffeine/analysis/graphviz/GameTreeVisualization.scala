package com.coinffeine.analysis.graphviz

import java.io.{File, PrintWriter}

import com.coinffeine.analysis._

/** Visualize a game tree using graphviz */
class GameTreeVisualization[Action, G <: Game[Action]](
    gameTree: G#GameTree,
    happyPath: Seq[Action] = Seq.empty,
    bestStrategies: Set[Seq[Action]] = Set.empty[Seq[Action]]) {

  private val happyPathStates: Set[G#State] = gameTree.root.historyOf(happyPath).toSet

  private val names: Map[G#State, String] = (for {
    (state, index) <- gameTree.stateSet.zipWithIndex
  } yield (state, s"node$index")).toMap

  val preferredTransitions: Set[(G#State, G#State)] = for {
    history <- bestStrategies
    historyStates = history.scanLeft(gameTree.root)((state, action) => state.play(action))
    List(sourceState, targetState) <- historyStates.sliding(2, 1)
  } yield sourceState -> targetState

  var out: PrintWriter = null

  def writeTo(file: String): Unit = writeTo(new File(file))

  def writeTo(file: File): Unit = {
    out = new PrintWriter(file)
    out.println("digraph {")
    printNodes()
    printEdges()
    out.println("}")
    out.close()
  }

  private def printNodes() {
    for (state <- gameTree.stateSet) {
      out.format("\t%s %s;\n", names(state), attributes(
        "label" -> labelOf(state),
        "color" -> (if (state.isFinal) "black"
                    else if (state.currentPlayer == Sam) "blue"
                    else "red"),
        "shape" -> (if (state.currentPlayer == Sam) "ellipse" else "box"),
        "fillcolor" -> (if (happyPathStates.contains(state)) "yellow" else "white"),
        "style" -> "filled"
      ))
    }
  }

  private def labelOf(state: G#State) = "%s [%.2f, %.2f]".format(
    if (state.isFinal) "Final" else state.currentPlayer,
    state.payoffs(Bob),
    state.payoffs(Sam)
  )

  private def printEdges() {
    for ((sourceState, edges) <- gameTree.tree;
         (action, targetState) <- edges) {
      val color = if (preferredTransitions.contains(sourceState -> targetState)) "green" else "black"
      out.format("\t%s -> %s %s;\n", names(sourceState), names(targetState), attributes(
        "label" -> action,
        "color" -> color,
        "fontcolor" -> color
      ))
    }
  }

  private def attributes(pairs: (String, Any)*) = pairs.map { pair =>
    "%s=\"%s\"".format(pair._1, pair._2)
  }.mkString("[", ",", "]")
}
