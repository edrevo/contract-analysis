package com.bitwiselabs.bitmarket.contractanalysis.upayments

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._
import scala.annotation.tailrec

object GameGraph {
  type Edges = Map[Action, State]
  type GameGraph = Map[State, Edges]
  type History = List[Action]
  val EmptyHistory: History = List.empty

  def generate(initialState: State): GameGraph = {

    @tailrec
    def visit(unseen: Seq[State], seen: GameGraph, maxDepth: Int = Integer.MAX_VALUE): GameGraph = {
      if (unseen.isEmpty) seen
      else if (maxDepth <= 0) seen ++ unseen.map(s => s -> Map.empty[Action, State]).toMap
      else {
        val current = unseen.head
        if (seen.contains(current)) visit(unseen.tail, seen, maxDepth)
        else {
          val children = (for {
            action <- Action.all if action.canPlay(current)
          } yield action -> action.play(current)).toMap
          val unseenChildren = children.values
          visit(unseen.tail ++ unseenChildren, seen ++ Map(current -> children), maxDepth - 1)
        }
      }
    }

    visit(Seq(initialState), Map.empty)
  }

  def resolveGraph(initialState: State, graph: GameGraph): Set[History] = {

    def resolve(state: State): Map[History, Payoffs] =
      if (graph(state).isEmpty) Map(EmptyHistory -> state.payoffs)
      else {
        val subGraphPayoffs: Seq[(Payoff, Action, Map[History, Payoffs])] = for {
          (action, childState) <- graph(state).toSeq
          bestStrategies = resolve(childState)
          minPayoff = bestStrategies.values.map(payoffs => payoffs(state.nextPlayer)).min
        } yield (minPayoff, action, bestStrategies)
        val bestMinPayoff = subGraphPayoffs.maxBy(_._1)._1
        (for {
          (`bestMinPayoff`, action, bestStrategies) <- subGraphPayoffs
          (history, payoffs) <- bestStrategies
        } yield (action :: history) -> payoffs).toMap
      }

    resolve(initialState).keySet
  }
}

object Action {
  val all: Seq[Action] = Seq(
    Enter(Sam),
    Enter(Bob),
    Wait(Sam),
    Wait(Bob),
    Offer,
    Sign,
    Pay,
    PayAndOffer,
    Publish
  )
}

sealed trait Action {
  val player: Player
  def canPlay(state: State): Boolean =
    (state.nextPlayer == player) && !(state.nextPlayer == Sam && state.unresponsiveSam) &&
      !state.timedOut && !state.lastOfferPublished
  def play(state: State): State
}

case class Enter(player: Player) extends Action {
  override def canPlay(state: State) = super.canPlay(state) && !state.uPaymentChannels.contains(player)
  def play(state: State): State = state.enter(player).changeTurn
}

case class Wait(player: Player) extends Action{
  override def canPlay(state: State) = super.canPlay(state) && state.uPaymentChannelsExist
  def play(state: State): State = player match {
    case Bob => state.copy(timedOut = true).changeTurn
    case Sam => state.copy(unresponsiveSam = true).changeTurn
  }
}

case object Publish extends Action {
  override val player = Bob
  override def canPlay(state: State) = super.canPlay(state) && state.lastSignedOffer.isDefined
  override def play(state: State): State = state.copy(lastOfferPublished = true).changeTurn
}

case object Offer extends Action {
  val defaultLastOffer: Payoffs = Map(Bob -> 0, Sam -> ContractAmount)
  override val player = Bob
  override def canPlay(state: State) = {
    val offer = nextOffer(state)
    val nonNegativeAmounts = offer.values.forall(_ >= 0)
    val feasibleTotalAmount = offer.values.sum <= ChannelAmounts.values.sum
    super.canPlay(state) && state.uPaymentChannelsExist && !state.unresponsiveSam && nonNegativeAmounts && feasibleTotalAmount
  }
  override def play(state: State): State = {
    state.copy(lastOffer = Some(nextOffer(state))).changeTurn
  }
  
  private def nextOffer(state: State) = {
    val newOffer = psum(state.lastOffer.getOrElse(defaultLastOffer), Map(Bob -> ContractStep, Sam -> (-ContractStep)))
    if (newOffer(Sam) == BigDecimal(0)) {
      psum(newOffer, Map(Bob -> ContractStep, Sam -> 0))
    } else {
      newOffer
    }
  }
}

case object Sign extends Action {
  override val player = Sam
  override def canPlay(state: State) = super.canPlay(state) && state.lastOffer.isDefined && state.lastOffer != state.lastSignedOffer
  override def play(state: State): State = state.signOffer.changeTurn
}

case object Pay extends Action {
  override val player = Bob
  override def canPlay(state: State) = super.canPlay(state) && state.uPaymentChannelsExist && state.amountPaid < ContractAmount
  override def play(state: State): State = state.pay.changeTurn
}

case object PayAndOffer extends Action {
  override val player = Bob
  override def canPlay(state: State) = {
    Pay.canPlay(state) && Offer.canPlay(Pay.play(state).changeTurn)
  }
  override def play(state: State): State = Offer.play(Pay.play(state).changeTurn)
}
