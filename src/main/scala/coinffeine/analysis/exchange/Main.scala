package coinffeine.analysis.exchange

import coinffeine.analysis.{Bob, AllowedActions, Sam}
import coinffeine.analysis.exchange.strategies.{StandardBob, StandardSam}
import coinffeine.analysis.graphviz.GameTreeVisualization

object Main {

  val parameters = Parameters()
  val game = new ExchangeGame(parameters)

  def analyzeCompleteGame() = game.completeTree()

  def analyzeStandardSamVersusRationalPlayer() = game.plausibleTree(
    Set(StandardSam.allowedActions, AllowedActions.any(Bob))
  )

  def analyzeStandardBobVersusRationalPlayer() = game.plausibleTree(
    Set(StandardBob.allowedActions, AllowedActions.any(Sam))
  )

  def standardGame() = game.plausibleTree(Set(StandardSam, StandardBob).map(_.allowedActions))

  def main(args: Array[String]) {
    val tree = standardGame()

    println("Game tree:")
    println(tree)
    println("Dominant strategies")
    println(tree.dominantStrategies)
    new GameTreeVisualization(tree, game.happyPath, tree.dominantStrategies).writeTo("protocol.dot")
  }
}
