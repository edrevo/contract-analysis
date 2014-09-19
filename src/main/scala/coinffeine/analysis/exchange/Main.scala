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
    val isValid = analyzeStandardBobVersusRationalPlayer().containsDesiredStrategy &&
      analyzeStandardSamVersusRationalPlayer().containsDesiredStrategy

    println(s"Is a valid mechanism design: $isValid")
    // new GameTreeVisualization(tree, game.desiredStrategy,
    //   tree.dominantStrategies).writeTo("protocol.dot")
  }
}
