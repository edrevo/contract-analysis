package com.bitwiselabs.bitmarket.contractanalysis.upayments

import org.scalatest.{MustMatchers, FlatSpec}

import com.bitwiselabs.bitmarket.contractanalysis.Player._
import Constants._

class StateTest extends FlatSpec with MustMatchers {

  "State payoffs" must "equal InitialValue for the initial state" in {
    State.initialState(Sam).payoffs must equal(InitialValues)
    State.initialState(Bob).payoffs must equal(InitialValues)
  }

  it must "be affected by the amount paid" in {
    State.initialState(Sam).pay.payoffs must equal (Map(
      Bob -> (InitialValues(Bob) - ContractStep),
      Sam -> (InitialValues(Sam) + ContractStep * (1 + ConsumerSurplus))
    ))
  }

  it must "be affected when transactions are published" in {
    val offer = Some(Map(
      Bob -> BigDecimal.valueOf(7),
      Sam -> BigDecimal.valueOf(5)
    ))
    val state = State.initialState(Sam).copy(
      lastOffer = offer,
      lastSignedOffer = offer,
      lastOfferPublished = true)
    state.payoffs must equal (Map(
      Bob -> (InitialValues(Bob) + (7 * (1 + ConsumerSurplus))),
      Sam -> (InitialValues(Sam) + 5)
    ))
  }
}
