// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
//
// RUN: echo '#line 1 "%s"' > "%t/main.swift" && cat "%s" >> "%t/main.swift" && chmod -w "%t/main.swift"
// RUN: %target-build-swift -Xfrontend -disable-access-control -I %S/Inputs/SlurpFastEnumeration/ %t/main.swift %S/Inputs/DictionaryKeyValueTypes.swift -Xlinker %t/SlurpFastEnumeration.o -o %t.out -O
// RUN: %target-run %t.out

// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import SlurpFastEnumeration

struct DictionaryBridge_objectForKey_RaceTest : RaceTestWithPerTrialDataType {
  class RaceData {
    var nsd: NSDictionary
    init(nsd: NSDictionary) {
      self.nsd = nsd
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UWord

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
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
  ) -> Observation {
    let nsd = raceData.nsd
    var v: AnyObject? = nsd.objectForKey(key)
    return Observation(unsafeBitCast(v, UWord.self))
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    sink.put(evaluateObservationsAllEqual(observations))
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
  typealias Observation = Observation4UWord

  func makeRaceData() -> RaceData {
    let nsd = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
      numElements: 2)
    return RaceData(nsd: nsd)
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
  ) -> Observation {
    let nsd = raceData.nsd
    let objcPairs = NSMutableArray()
    slurpFastEnumerationOfDictionaryFromObjCImpl(nsd, nsd, objcPairs)
    return Observation(
      unsafeBitCast(objcPairs[0], UWord.self),
      unsafeBitCast(objcPairs[1], UWord.self),
      unsafeBitCast(objcPairs[2], UWord.self),
      unsafeBitCast(objcPairs[3], UWord.self))
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    sink.put(evaluateObservationsAllEqual(observations))
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

