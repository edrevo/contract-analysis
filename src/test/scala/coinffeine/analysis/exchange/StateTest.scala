package coinffeine.analysis.exchange

import org.scalatest.{FlatSpec, MustMatchers}

import coinffeine.analysis.{Bob, Payoffs}

class StateTest extends FlatSpec with MustMatchers {

  import Parameters.default._
  val initialState = ExchangeState.initial(Parameters.default)

  "State payoffs" must "equal initialValue for the initial state" in {
    initialState.payoffs must equal(initialValues)
  }

  it must "be affected by the amount paid" in {
    initialState.playAll(Seq(Enter, Enter, Pay)).payoffs must equal (
      fiatAmounts + Payoffs(bob = -contractStep, sam = contractStep)
    )
  }

  it must "be affected when transactions are published" in {
    val state = ExchangingState(currentPlayer = Bob, stepsPaid = 0, txSigned = 8)(Parameters.default)
    state.play(Publish).payoffs must equal (fiatAmounts + Payoffs(
      bob = 7 * contractStep,
      sam = 3 * contractStep
    ))
  }
}
