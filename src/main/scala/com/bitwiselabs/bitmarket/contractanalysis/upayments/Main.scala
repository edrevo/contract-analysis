package com.bitwiselabs.bitmarket.contractanalysis.upayments

import java.io.File

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._

object Main {

  def main(args: Array[String]) {
    val initialState = State.initialState(Bob)
    val graph = GameGraph.generate(initialState)
    val actions: Seq[Action] =
      Seq(Enter(Bob), Enter(Sam), Offer, Sign) ++ Seq.fill(N - 1)(Seq(PayAndOffer, Sign)).flatten ++ Seq(Publish)
    val happyPath: Seq[State] = actions.scanLeft(initialState)((state, action) => action.play(state))
    val bestStrategies = GameGraph.resolveGraph(initialState, graph)
    new GameGraphviz(initialState, graph, happyPath, bestStrategies).writeTo(new File("uPayment.dot"))
  }
}
