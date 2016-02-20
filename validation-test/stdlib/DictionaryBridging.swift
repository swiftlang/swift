// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: echo '#line 1 "%s"' > "%t/main.swift" && cat "%s" >> "%t/main.swift" && chmod -w "%t/main.swift"
// RUN: %target-build-swift -Xfrontend -disable-access-control -I %S/Inputs/SlurpFastEnumeration/ %t/main.swift %S/Inputs/DictionaryKeyValueTypes.swift -Xlinker %t/SlurpFastEnumeration.o -o %t.out -O
// RUN: %target-run %t.out
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import SlurpFastEnumeration

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

struct DictionaryBridge_objectForKey_RaceTest : RaceTestWithPerTrialDataType {
  class RaceData {
    var nsd: NSDictionary
    init(nsd: NSDictionary) {
      self.nsd = nsd
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UInt

  func makeRaceData() -> RaceData {
    let nsd = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
      numElements: 1)
    return RaceData(nsd: nsd)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  let key = TestObjCKeyTy(10)

  func thread1(
    raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let nsd = raceData.nsd
    let v: AnyObject? = nsd.objectForKey(key)
    return Observation(unsafeBitCast(v, UInt.self))
  }

  func evaluateObservations(
    observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

struct DictionaryBridge_KeyEnumerator_FastEnumeration_ObjC_RaceTest :
  RaceTestWithPerTrialDataType {
  class RaceData {
    var nsd: NSDictionary
    init(nsd: NSDictionary) {
      self.nsd = nsd
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation4UInt

  func makeRaceData() -> RaceData {
    let nsd = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
      numElements: 2)
    return RaceData(nsd: nsd)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    let nsd = raceData.nsd
    let objcPairs = NSMutableArray()
    slurpFastEnumerationOfDictionaryFromObjCImpl(nsd, nsd, objcPairs)
    return Observation(
      unsafeBitCast(objcPairs[0], UInt.self),
      unsafeBitCast(objcPairs[1], UInt.self),
      unsafeBitCast(objcPairs[2], UInt.self),
      unsafeBitCast(objcPairs[3], UInt.self))
  }

  func evaluateObservations(
    observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

var DictionaryTestSuite = TestSuite("Dictionary")

DictionaryTestSuite.test(
  "BridgedToObjC.KeyValue_ValueTypesCustomBridged/RaceTest") {
  runRaceTest(DictionaryBridge_objectForKey_RaceTest.self, trials: 100)
}

DictionaryTestSuite.test(
  "BridgedToObjC.Custom.KeyEnumerator.FastEnumeration.UseFromObjC/RaceTest") {
  runRaceTest(
    DictionaryBridge_KeyEnumerator_FastEnumeration_ObjC_RaceTest.self,
    trials: 100)
}

DictionaryTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

DictionaryTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

runAllTests()

