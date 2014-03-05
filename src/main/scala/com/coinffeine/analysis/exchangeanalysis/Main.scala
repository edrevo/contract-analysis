package com.coinffeine.analysis.exchangeanalysis

import java.io.File

import com.coinffeine.analysis.exchangeanalysis.actions.Action

object Main extends App {
  println("Generating game...")
  val graph = GameGraph.generate(Action.initialState)
  println("Game generated!")
  val resolutionInitialState = Action.happyStates.drop(0).head
  println("Resolving game...")
  val bestStrategies = GameGraph.resolveGraph(resolutionInitialState, graph)
  println("Game resolved!")
  new GameGraphviz(resolutionInitialState, graph, Action.happyStates, bestStrategies).writeTo(new File("uPayment.dot"))
}
