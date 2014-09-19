package coinffeine.analysis.exchange

import coinffeine.analysis._

sealed trait ExchangeState extends GameState[ExchangeAction] {
  override def toString = "%s[%s]".format(currentName, description)
  private def currentName: String = if (isFinal) "Final" else currentPlayer.toString
  protected def description: String
}

/** Base class for endgame states */
abstract class FinalState(override val description: String, override val payoffs: Payoffs)
  extends ExchangeState {
  override val actions: Set[ExchangeAction] = Set.empty
  override def play(action: ExchangeAction) =
    throw new IllegalArgumentException("No action is allowed")
}

/** Players should enter or leave the exchange */
case class HandshakeState(override val currentPlayer: Player,
                          playersInUPaymentChannels: Set[Player] = Set.empty)
                         (implicit val parameters: Parameters) extends ExchangeState {

  import parameters._

  override def actions = if (hasEntered(currentPlayer)) Set.empty else Set(Enter, Leave)

  override def payoffs = initialValues

  override def play(action: ExchangeAction) = action match {
    case Enter =>
      if (playersInUPaymentChannels.size == 1) ExchangingState(currentPlayer.otherPlayer)
      else HandshakeState(currentPlayer.otherPlayer, playersInUPaymentChannels + currentPlayer)
    case Leave =>
      AbortedState(currentPlayer.otherPlayer)
    case _ => throw new IllegalArgumentException("Invalid action")
  }

  private def hasEntered(player: Player): Boolean = playersInUPaymentChannels.contains(player)

  override protected val description =
    if (playersInUPaymentChannels.isEmpty) "start"
    else  s"handshaking(${playersInUPaymentChannels.head} is in)"
}

/** No proper exchanged started. */
case class AbortedState(override val currentPlayer: Player)(implicit val parameters: Parameters)
  extends FinalState("aborted", payoffs = parameters.initialValues)

/** Game ended when timelocks of refund transactions expired and were published. */
case class TimedOutState(amountPaid: Payoff)(implicit parameters: Parameters)
  extends FinalState(
    description = s"timeout ($amountPaid paid)",
    payoffs = {
      import parameters._
      initialBalances.pay(amountPaid).loseBtc(refundPenalization).utilities(consumerSurplus)
    }
  ) {
  override val currentPlayer = Sam
}

/** Game ended by publishing a u-payment channel transaction */
case class PublishedState(amountPaid: Payoff, stepsSigned: Int)
                         (implicit val parameters: Parameters) extends FinalState(
  description = {
    val txName = if (stepsSigned > parameters.steps) "TXfinal" else s"TX$stepsSigned"
    s"published $txName ($amountPaid paid)"
  },
  payoffs = {
    import parameters._
    Balances(btc = offers(stepsSigned - 1), fiat = fiatAmounts, parameters.feePolicy).pay(amountPaid)
      .utilities(consumerSurplus)
  }
) {
  override val currentPlayer: Player = Sam
}

/** Represents a game during the exchange phase.
  *
  * @constructor
  * @param currentPlayer    The player that will play the next action
  * @param stepsPaid        How much fiat money has Bob paid to Sam (in steps)
  * @param txSigned         The last of Bob's offers that Sam signed
  * @param unresponsiveSam  Whether Sam has decided to abandon the game
  * @param parameters       Exchange parameters
  */
case class ExchangingState(
    override val currentPlayer: Player,
    stepsPaid: Int = 0,
    txSigned: Int = 0,
    unresponsiveSam: Boolean = false)(implicit val parameters: Parameters) extends ExchangeState {

  import parameters._

  require(stepsPaid >= 0 && stepsPaid <= steps)
  require(txSigned >= 0 && txSigned <= steps + 1)

  override lazy val actions: Set[ExchangeAction] =
    if (currentPlayer == Sam) samActions else bobActions

  private def samActions = enabledActions(
    (!unresponsiveSam && txSigned < steps + 1) -> Sign,
    (!unresponsiveSam) -> Wait
  )

  private def bobActions = enabledActions(
    (amountPaid < contractAmount && !unresponsiveSam) -> Pay,
    (txSigned > 0) -> Publish
  ) + Wait

  private def enabledActions(pairs: (Boolean, ExchangeAction)*) = pairs.collect {
    case (cond, action) if cond => action
  }.toSet

  override def play(action: ExchangeAction): ExchangeState = action match {
    case Pay =>
      copy(stepsPaid = stepsPaid + 1).changeTurn
    case Publish =>
      PublishedState(amountPaid, txSigned)
    case Sign =>
      copy(txSigned = txSigned + 1).changeTurn
    case Wait =>
      currentPlayer match {
        case Bob => TimedOutState(amountPaid)
        case Sam => copy(currentPlayer = Bob, unresponsiveSam = true)
      }
    case _ =>
      throw new IllegalArgumentException("Invalid action")
  }

  override def payoffs: Payoffs =
    Balances(btc = Payoffs.zero, fiat = fiatAmounts, parameters.feePolicy).pay(amountPaid)
      .utilities(consumerSurplus)

  /** True if the actual exchange can begin */
  def uPaymentChannelsExist: Boolean = true

  def changeTurn =
    if (unresponsiveSam && currentPlayer == Sam) this
    else copy(currentPlayer = currentPlayer.otherPlayer)


  def amountPaid: Payoff =
    if (stepsPaid == steps) parameters.contractAmount
    else stepsPaid * parameters.contractStep

  override protected def description: String = {
    val flag = if (unresponsiveSam) Some("unresponsive-sam") else None
    (Seq(s"paid=$amountPaid", s"signed=$txSigned") ++ flag).mkString(", ")
  }
}

object ExchangeState {
  def initial(parameters: Parameters): ExchangeState = new HandshakeState(Sam)(parameters)
}
