// RUN: %target-run-simple-swift

import SwiftPrivate
import StdlibUnittest
import Foundation

enum SomeError: _ErrorType {
  case GoneToFail
}

struct ErrorTypeAsNSErrorRaceTest: RaceTestWithPerTrialDataType {
  class RaceData {
    let error: _ErrorType

    init(error: _ErrorType) {
      self.error = error
    }
  }

  func makeRaceData() -> RaceData {
    return RaceData(error: SomeError.GoneToFail)
  }

  func makeThreadLocalData() {}

  func thread1(raceData: RaceData, inout _: Void) -> Observation3Word {
    let ns = raceData.error as NSError
    let domainWord: Word = unsafeBitCast(ns.domain as NSString, Word.self)
    let code: Word = ns.code
    let userInfoWord: Word = unsafeBitCast(ns.userInfo as NSDictionary?, Word.self)
    return Observation3Word(domainWord, code, userInfoWord)
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation3Word>, inout _ sink: S) {
    if observations.isEmpty { return }

    let first = observations[0]
    sink.put(.Pass)
    for i in 1..<observations.count {
      if observations[i] == first {
        sink.put(.Pass)
      } else {
        sink.put(.Failure)
      }
    }
  }
}

var ErrorTypeRaceTestSuite = TestSuite("ErrorType races")
ErrorTypeRaceTestSuite.test("NSError bridging") {
  runRaceTest(ErrorTypeAsNSErrorRaceTest.self, operations: 1000)
}
