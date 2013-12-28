package com.bitwiselabs.bitmarket.contractanalysis.upayments

import java.io.File

object Main {

  def main(args: Array[String]) {
    println("Generating game...")
    val graph = GameGraph.generate(Action.initialState)
    println("Game generated!")
    val resolutionInitialState = Action.happyStates.drop(0).head
    println("Resolving game...")
    val bestStrategies = GameGraph.resolveGraph(resolutionInitialState, graph)
    println("Game resolved!")
    new GameGraphviz(resolutionInitialState, graph, Action.happyStates, bestStrategies).writeTo(new File("uPayment.dot"))
  }
}
