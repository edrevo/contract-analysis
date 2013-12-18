package com.bitwiselabs.bitmarket.contractanalysis

import java.io.File

import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Main {

  val optimalSeq: History = List(
    EnterDeposit(Bob),
    EnterDeposit(Sam),
    TransferMoney,
    SignDepositA(Sam),
    SignDepositA(Bob),
    SignDepositB(Sam),
    SignDepositB(Bob),
    StopPlaying(Sam)
  )

  def main(args: Array[String]) {
    val initialState = State(Bob)
    println("Generating game tree...")
    val gameTree = generateGameTree(initialState)
    println("Done")
    println(s"Initial state: $initialState")
    println(s"Desired play moves: ${optimalSeq.mkString("\n ")}")
    println()
    println("Resolving tree...")
    val resolvedTree = resolveTree(gameTree, initialState)
    println("Is the desired play moves a dominant strategy? " +
      resolvedTree.contains(optimalSeq))
    //println(resolveTree(gameTree, initialState).mkString("\n"))

    new GameGraph(initialState, gameTree, resolvedTree).writeTo(new File("./game.dot"))
  }

  def generateGameTree(initialState: State): Map[State, MoveMap] = {
    var result: Map[State, MoveMap] = Map()
    var unprocessedStates: Set[State] = Set(initialState)
    while (unprocessedStates.nonEmpty) {
      val state = unprocessedStates.head
      val moveMap = (for {
        move <- Move.moves
        if move.canPlay(state)
      } yield (move, move(state))).toMap
      val unseenStates = moveMap.values.filterNot(result.keySet.contains).toSet
      unprocessedStates = (unprocessedStates ++ unseenStates) - state
      result = result.updated(state, moveMap)
    }
    result
  }

  def resolveTree(
      tree: Map[State, MoveMap],
      state: State,
      seen: Set[State] = Set()): Map[History, Payoff] =
    if(tree(state).isEmpty) {
      Map(EmptyHistory -> state.payoff)
    } else if (seen.contains(state)) {
      Map(EmptyHistory -> state.payoff)
    } else {
      val moveMap = tree(state)
      val subGamePayoffs = for {
        (move, newState) <- moveMap
      } yield {
        val subGameResolution = resolveTree(tree, newState, seen + state)
        val minPayoff = subGameResolution.values.map(payoff => payoff(state.playerTurn)).min
        (move, subGameResolution) -> minPayoff
      }
      val maxMinPlayerPayoff = subGamePayoffs.maxBy(_._2)._2
      for {
        ((move, resolution), minPayoff) <- subGamePayoffs
        if minPayoff == maxMinPlayerPayoff
        (history, payoff) <- resolution
      } yield (move :: history).toList -> payoff
    }
}
