// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: echo '#line 1 "%s"' > "%t/main.swift" && cat "%s" >> "%t/main.swift" && chmod -w "%t/main.swift"
// RUN: %target-build-swift -Xfrontend -disable-access-control -I %S/Inputs/SlurpFastEnumeration/ %t/main.swift %S/Inputs/DictionaryKeyValueTypes.swift -Xlinker %t/SlurpFastEnumeration.o -o %t.out -O
// RUN: %target-run %t.out
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: executable_test

import StdlibUnittest
import Foundation
import SlurpFastEnumeration

// Also import modules which are used by StdlibUnittest internally. This is
// needed to link all required libraries in case we serialize StdlibUnittest.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

struct ArrayBridge_objectAtIndex_RaceTest : RaceTestWithPerTrialDataType {
  class RaceData {
    var nsa: NSArray
    init(nsa: NSArray) {
      self.nsa = nsa
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UInt

  func makeRaceData() -> RaceData {
    let nsa = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 1)
    return RaceData(nsa: nsa)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
  ) -> Observation {
    let nsa = raceData.nsa
    let v: AnyObject = nsa.objectAtIndex(0)
    return Observation(unsafeBitCast(v, UInt.self))
  }

  func evaluateObservations(
    observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

struct ArrayBridge_FastEnumeration_ObjC_RaceTest :
  RaceTestWithPerTrialDataType {
  class RaceData {
    var nsa: NSArray
    init(nsa: NSArray) {
      self.nsa = nsa
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation4UInt

  func makeRaceData() -> RaceData {
    let nsa = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 4)
    return RaceData(nsa: nsa)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
  ) -> Observation {
    let nsa = raceData.nsa
    let objcValues = NSMutableArray()
    slurpFastEnumerationOfArrayFromObjCImpl(nsa, nsa, objcValues)
    return Observation(
      unsafeBitCast(objcValues[0], UInt.self),
      unsafeBitCast(objcValues[1], UInt.self),
      unsafeBitCast(objcValues[2], UInt.self),
      unsafeBitCast(objcValues[3], UInt.self))
  }

  func evaluateObservations(
    observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var ArrayTestSuite = TestSuite("Array")

ArrayTestSuite.test(
  "BridgedToObjC/Custom/objectAtIndex/RaceTest") {
  runRaceTest(ArrayBridge_objectAtIndex_RaceTest.self, trials: 100)
}

ArrayTestSuite.test(
  "BridgedToObjC/Custom/FastEnumeration/UseFromObjC/RaceTest") {
  runRaceTest(
    ArrayBridge_FastEnumeration_ObjC_RaceTest.self,
    trials: 100)
}

ArrayTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

ArrayTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

runAllTests()

