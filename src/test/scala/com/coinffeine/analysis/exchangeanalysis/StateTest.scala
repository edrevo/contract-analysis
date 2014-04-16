package com.coinffeine.analysis.exchangeanalysis

import org.scalatest.{MustMatchers, FlatSpec}

import com.coinffeine.analysis.exchangeanalysis.Player._
import com.coinffeine.analysis.exchangeanalysis.Constants._

class StateTest extends FlatSpec with MustMatchers {

  "State payoffs" must "equal InitialValue for the initial state" in {
    State.initialState(Sam).payoffs must equal(InitialValues)
    State.initialState(Bob).payoffs must equal(InitialValues)
  }

  it must "be affected by the amount paid" in {
    State.initialState(Sam).pay.payoffs must equal (Payoffs(
      bob = InitialValues(Bob) - ContractStep,
      sam = InitialValues(Sam) + ContractStep * (1 + ConsumerSurplus)
    ))
  }

  it must "be affected when transactions are published" in {
    val offer = Some(Payoffs(
      bob = BigDecimal.valueOf(7),
      sam = BigDecimal.valueOf(5)
    ))
    val state = State.initialState(Sam).copy(
      lastSignedOffer = offer,
      lastOfferPublished = true)
    state.payoffs must equal (Payoffs(
      bob = InitialValues(Bob) + (7 * (1 + ConsumerSurplus)),
      sam = InitialValues(Sam) + 5
    ))
  }
}
