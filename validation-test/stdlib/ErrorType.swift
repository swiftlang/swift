// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import SwiftPrivate
import StdlibUnittest
import Foundation

enum SomeError : ErrorType {
  case GoneToFail
}

struct ErrorTypeAsNSErrorRaceTest : RaceTestWithPerTrialDataType {
  class RaceData {
    let error: ErrorType

    init(error: ErrorType) {
      self.error = error
    }
  }

  func makeRaceData() -> RaceData {
    return RaceData(error: SomeError.GoneToFail)
  }

  func makeThreadLocalData() {}

  func thread1(raceData: RaceData, inout _: Void) -> Observation3Word {
    let ns = raceData.error as NSError
    // Use valueForKey to bypass bridging, so we can verify that the identity
    // of the unbridged NSString object is stable.
    let domainWord: Word = unsafeBitCast(ns.valueForKey("domain"), Word.self)
    let code: Word = ns.code
    let userInfoWord: Word = unsafeBitCast(ns.valueForKey("userInfo"), Word.self)
    return Observation3Word(domainWord, code, userInfoWord)
  }

  func evaluateObservations(observations: Array<Observation3Word>,
      _ sink: (RaceTestObservationEvaluation) -> ()) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var ErrorTypeRaceTestSuite = TestSuite("ErrorType races")
ErrorTypeRaceTestSuite.test("NSError bridging") {
  runRaceTest(ErrorTypeAsNSErrorRaceTest.self, operations: 1000)
}
runAllTests()
