// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import SwiftPrivate
import StdlibUnittest
import Foundation

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

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

  func thread1(raceData: RaceData, inout _: Void) -> Observation3Int {
    let ns = raceData.error as NSError
    // Use valueForKey to bypass bridging, so we can verify that the identity
    // of the unbridged NSString object is stable.
    let domainInt: Int = unsafeBitCast(ns.valueForKey("domain"), Int.self)
    let code: Int = ns.code
    let userInfoInt: Int = unsafeBitCast(ns.valueForKey("userInfo"), Int.self)
    return Observation3Int(domainInt, code, userInfoInt)
  }

  func evaluateObservations(
    observations: [Observation3Int],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var ErrorTypeRaceTestSuite = TestSuite("ErrorType races")
ErrorTypeRaceTestSuite.test("NSError bridging") {
  runRaceTest(ErrorTypeAsNSErrorRaceTest.self, operations: 1000)
}
runAllTests()
