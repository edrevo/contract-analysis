package com.bitwiselabs.bitmarket.contractanalysis

import java.io.File

import com.bitwiselabs.bitmarket.contractanalysis.Constants._
import com.bitwiselabs.bitmarket.contractanalysis.Player._

object Main {

  val optimalSeq: History =
    if (hasBobDepositA) List(
      EnterDeposit(Bob),
      EnterDeposit(Sam),
      TransferMoney,
      SignDepositA(Sam),
      SignDepositA(Bob),
      SignDepositB(Sam),
      SignDepositB(Bob),
      StopPlaying(Sam)
    ) else List(
      EnterDeposit(Sam),
      EnterDeposit(Bob),
      SignDepositB(Sam),
      TransferMoney,
      SignDepositA(Sam),
      SignDepositB(Bob),
      StopPlaying(Sam)
    )

  val startFromScratch = State(if (hasBobDepositA) Bob else Sam)
  val startWithDeposits = Move.sequence(optimalSeq.take(2)).apply(startFromScratch)

  def main(args: Array[String]) {
    val initialState = startFromScratch
    println("Generating game tree...")
    val gameTree = generateGameTree(initialState)
    println("Done")
    println(s"Initial state: $initialState")
    println(s"Desired play moves: ${optimalSeq.mkString("\n ")}")
    println()
    println("Resolving tree...")
    val bestStrategies = resolveTree(gameTree, initialState)
    println("Is the desired play moves a dominant strategy? " +
      bestStrategies.contains(optimalSeq))
    
    printBestStrategies(bestStrategies)
    new GameGraph(initialState, gameTree, bestStrategies).writeTo(new File("./game.dot"))
  }

  def printBestStrategies(strategies: Map[History, Payoff]) {
    println("Best strategies:")
    for (((moves, payoff), index) <- strategies.zipWithIndex) {
      println(s"\t$index: [${payoff(Bob)}, ${payoff(Sam)}] ${moves.map(_.toShortString).mkString(", ")}")
    }
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
    if (tree(state).isEmpty || seen.contains(state)) Map(EmptyHistory -> state.payoff)
    else {
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

