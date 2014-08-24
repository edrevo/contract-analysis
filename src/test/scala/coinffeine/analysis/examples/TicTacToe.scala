package com.coinffeine.analysis.examples

import coinffeine.analysis._

object TicTacToe {

  case class Position(x: Int, y: Int) {
    require(x >= 0 && x < 3)
    require(y >= 0 && y < 3)

    override def toString = s"($x, $y)"
  }

  object Position {
    val all: Set[Position] = (for {
      x <- 0 to 2
      y <- 0 to 2
    } yield Position(x, y)).toSet
  }

  case class Board(override val currentPlayer: Player, cells: Map[Position, Player])
    extends GameState[Position] {

    override def actions: Set[Position] = Position.all.filter(cells.get(_).isEmpty)

    override def play(action: Position): Board =
      Board(currentPlayer.otherPlayer, cells + (action -> currentPlayer))

    override def toString = formatTable(Seq.tabulate(3, 3) { (x, y) =>
      cells.get(Position(x, y)).fold("-")(_.toString)
    })

    private def formatTable(table: Seq[Seq[String]]): String =
      table.map(formatRow).mkString("[", "|", "]")

    val winPayoff: Payoff = 1
    val losePayoff: Payoff = -1
    val drawPayoff: Payoff = 0
    override def payoffs = winner.fold(Payoffs(drawPayoff, drawPayoff)) { winner =>
      Payoffs(winner -> winPayoff, winner.otherPlayer -> losePayoff)
    }

    private def winner: Option[Player] = (for {
      line <- Board.lines
      contents = line.toSeq.flatMap(pos => cells.get(pos))
      if samePlayer(contents)
    } yield contents.head).headOption

    private def samePlayer(row: Seq[Player]) =
      if (row.isEmpty) false
      else row.forall(_ == row.head)

    private def formatRow(row: Seq[String]): String = row.mkString("")
  }
  object Board {
    val empty = Board(Sam, Map.empty)

    private val range = (0 to 2).toSet[Int]
    private val rows = range.map(row => line(0, row, advanceX = 1))
    private val cols = range.map(col => line(col, 0, advanceY = 1))
    private val diagonals = Set(line(0, 0, 1, 1), line(0, 2, 1, -1))
    private val lines = rows ++ cols ++ diagonals

    private def line(startX: Int, startY: Int, advanceX: Int = 0, advanceY: Int = 0): Set[Position] =
      range.map(idx => Position(startX + idx * advanceX, startY + idx * advanceY))
  }

  val game = Game[Position](Board.empty)

  def main(args: Array[String]): Unit = {
    println("Game tree:")
    println(game.completeTree().dominantStrategies)
  }
}
