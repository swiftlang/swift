// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: echo '#sourceLocation(file: "%s", line: 1)' > "%t/main.swift" && cat "%s" >> "%t/main.swift" && chmod -w "%t/main.swift"
// RUN: %target-build-swift -Xfrontend -disable-access-control -I %S/Inputs/SlurpFastEnumeration/ %t/main.swift %S/Inputs/DictionaryKeyValueTypes.swift %S/Inputs/DictionaryKeyValueTypesObjC.swift -Xlinker %t/SlurpFastEnumeration.o -o %t.out -O
// RUN: %target-run %t.out
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: nonatomic_rc

import StdlibUnittest
import Foundation
import SlurpFastEnumeration


struct ArrayBridge_objectAtIndex_RaceTest : RaceTestWithPerTrialData {
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
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let nsa = raceData.nsa
    let v = nsa.object(at: 0) as AnyObject
    return Observation(unsafeBitCast(v, to: UInt.self))
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

struct ArrayBridge_FastEnumeration_ObjC_RaceTest :
  RaceTestWithPerTrialData {
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
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let nsa = raceData.nsa
    let objcValues = NSMutableArray()
    slurpFastEnumerationOfArrayFromObjCImpl(nsa, nsa, objcValues)
    return Observation(
      unsafeBitCast(objcValues[0] as AnyObject, to: UInt.self),
      unsafeBitCast(objcValues[1] as AnyObject, to: UInt.self),
      unsafeBitCast(objcValues[2] as AnyObject, to: UInt.self),
      unsafeBitCast(objcValues[3] as AnyObject, to: UInt.self))
  }

  func evaluateObservations(
    _ observations: [Observation],
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
  resetLeaksOfObjCDictionaryKeysValues()
}

ArrayTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
  expectNoLeaksOfObjCDictionaryKeysValues()
}

runAllTests()

