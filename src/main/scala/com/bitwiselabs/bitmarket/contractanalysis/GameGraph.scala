package com.bitwiselabs.bitmarket.contractanalysis

import java.io.{PrintWriter, File}

import Player._

class GameGraph(
    initialState: State,
    gameTree: Map[State, MoveMap],
    resolvedTree: Map[History, Payoff]) {

  val names: Map[State, String] = (for {
    (state, index) <- gameTree.keySet.zipWithIndex
  } yield state -> s"node$index").toMap

  val preferredTransitions: Set[(State, State)] = {
    for {
      history <- resolvedTree.keySet
      historyStates = history.scanLeft(initialState)((state, move) => move(state))
      List(sourceState, targetState) <- historyStates.sliding(2, 1)
    } yield sourceState -> targetState
  }

  var out: PrintWriter = null

  def writeTo(file: File) {
    out = new PrintWriter(file)
    out.println("digraph {")
    printNodes()
    printEdges()
    out.println("}")
    out.close()
  }

  private def printNodes() {
    for (state <- gameTree.keys;
         name = names(state)) {
      out.format("\t%s %s;\n", name, attributes(
        "label" -> labelOf(state),
        "color" -> (if (state.playerTurn == Sam) "blue" else "red"),
        "shape" -> (if (state.playerTurn == Sam) "ellipse" else "box")
      ))
    }
  }

  private def printEdges() {
    for ((sourceState, moveMap) <- gameTree;
         (action, targetState) <- moveMap) {
      out.format("\t%s -> %s %s;\n", names(sourceState), names(targetState), attributes(
        "label" -> action.toShortString,
        "color" -> (if (preferredTransitions.contains(sourceState -> targetState)) "green" else "black")
      ))
    }
  }

  private def labelOf(state: State) =
    "%s [%d, %d]".format(state.playerTurn, state.payoff(Bob), state.payoff(Sam))

  private def attributes(pairs: (String, Any)*) = pairs.map { pair =>
    "%s=\"%s\"".format(pair._1, pair._2)
  }.mkString("[", ",", "]")
}
