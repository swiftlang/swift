// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: nonatomic_rc

import SwiftPrivate
import StdlibUnittest
import Foundation


enum SomeError : Error {
  case GoneToFail
}

struct ErrorAsNSErrorRaceTest : RaceTestWithPerTrialData {
  class RaceData {
    let error: Error

    init(error: Error) {
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
    let domainInt: Int = unsafeBitCast(ns.value(forKey: "domain").map { $0 as AnyObject },
                                       to: Int.self)
    let code: Int = ns.code
    let userInfoInt: Int = unsafeBitCast(ns.value(forKey: "userInfo").map { $0 as AnyObject },
                                         to: Int.self)
    return Observation3Int(domainInt, code, userInfoInt)
  }

  func evaluateObservations(
    _ observations: [Observation3Int],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var ErrorRaceTestSuite = TestSuite("Error races")
ErrorRaceTestSuite.test("NSError bridging") {
  runRaceTest(ErrorAsNSErrorRaceTest.self, operations: 1000)
}
runAllTests()
