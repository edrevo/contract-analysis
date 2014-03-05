package com.coinffeine.analysis.exchangeanalysis

import java.io.{PrintWriter, File}

import com.coinffeine.analysis.exchangeanalysis.Player._
import GameGraph._

/** Generates a DOT file for a given graph. This file can be opened with GraphViz and can help
  * visualizing how different parameters affect the game dynamics.
  */
class GameGraphviz(
    initialState: State,
    game: GameGraph,
    happyPath: Seq[State] = Seq.empty,
    bestStrategies: Set[History] = Set.empty) {

  val names: Map[State, String] = (for {
    (state, index) <- game.keys.zipWithIndex
  } yield (state, s"node$index")).toMap

  val preferredTransitions: Set[(State, State)] = for {
    history <- bestStrategies
    historyStates = history.scanLeft(initialState)((state, action) => action.play(state))
    List(sourceState, targetState) <- historyStates.sliding(2, 1)
  } yield sourceState -> targetState

  var out: PrintWriter = null

  def writeTo(file: File) {
    out = new PrintWriter(file)
    out.println("digraph {")
    printNodes()
    printEdges()
    out.println("}")
    out.close()
  }

  def isFinal(state: State) = game(state).isEmpty

  private def printNodes() {
    for (state <- game.keys;
         name = names(state)) {
      out.format("\t%s %s;\n", name, attributes(
        "label" -> labelOf(state),
        "color" -> (if (isFinal(state)) "black"
                    else if (state.nextPlayer == Sam) "blue"
                    else "red"),
        "shape" -> (if (state.nextPlayer == Sam) "ellipse" else "box"),
        "fillcolor" -> (if (happyPath.contains(state)) "yellow" else "white"),
        "style" -> "filled"
      ))
    }
  }

  private def labelOf(state: State) = "%s [%.2f, %.2f]".format(
    if (isFinal(state)) "Final" else state.nextPlayer,
    state.payoffs(Bob),
    state.payoffs(Sam)
  )

  private def printEdges() {
    for ((sourceState, edges)<- game;
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
