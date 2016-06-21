// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import SwiftPrivate
import StdlibUnittest
import Foundation


enum SomeError : ErrorProtocol {
  case GoneToFail
}

struct ErrorProtocolAsNSErrorRaceTest : RaceTestWithPerTrialData {
  class RaceData {
    let error: ErrorProtocol

    init(error: ErrorProtocol) {
      self.error = error
    }
  }

  func makeRaceData() -> RaceData {
    return RaceData(error: SomeError.GoneToFail)
  }

  func makeThreadLocalData() {}

  func thread1(_ raceData: RaceData, _: inout Void) -> Observation3Int {
    let ns = raceData.error as NSError
    // Use valueForKey to bypass bridging, so we can verify that the identity
    // of the unbridged NSString object is stable.
    let domainInt: Int = unsafeBitCast(ns.value(forKey: "domain"), to: Int.self)
    let code: Int = ns.code
    let userInfoInt: Int = unsafeBitCast(ns.value(forKey: "userInfo"), to: Int.self)
    return Observation3Int(domainInt, code, userInfoInt)
  }

  func evaluateObservations(
    _ observations: [Observation3Int],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var ErrorProtocolRaceTestSuite = TestSuite("ErrorProtocol races")
ErrorProtocolRaceTestSuite.test("NSError bridging") {
  runRaceTest(ErrorProtocolAsNSErrorRaceTest.self, operations: 1000)
}
runAllTests()
