package com.bitwiselabs.bitmarket.contractanalysis.upayments

import java.io.File

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._

object Main {

  def main(args: Array[String]) {
    val initialState = State.initialState(Bob)
    val graph = GameGraph.generate(initialState)
    val actions: Seq[Action] =
      Seq(Enter(Bob), Enter(Sam), Offer, Sign) ++ Seq.fill(N - 1)(Seq(PayAndOffer, Sign)).flatten ++ Seq(Pay, Wait(Sam), Publish)
    val happyPath: Seq[State] = actions.scanLeft(initialState)((state, action) => action.play(state))
    val resolutionInitialState = happyPath.drop(0).head
    val bestStrategies = GameGraph.resolveGraph(resolutionInitialState, graph)
    new GameGraphviz(resolutionInitialState, graph, happyPath, bestStrategies).writeTo(new File("uPayment.dot"))
  }
}
