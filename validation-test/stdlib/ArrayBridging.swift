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

struct ArrayBridge_objectAtIndex_RaceTest : RaceTestWithPerTrialDataType {
  class RaceData {
    var nsa: NSArray
    init(nsa: NSArray) {
      self.nsa = nsa
    }
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UWord

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
    var v: AnyObject = nsa.objectAtIndex(0)
    return Observation(unsafeBitCast(v, UWord.self))
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    sink.put(evaluateObservationsAllEqual(observations))
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
  typealias Observation = Observation4UWord

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
      unsafeBitCast(objcValues[0], UWord.self),
      unsafeBitCast(objcValues[1], UWord.self),
      unsafeBitCast(objcValues[2], UWord.self),
      unsafeBitCast(objcValues[3], UWord.self))
  }

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    sink.put(evaluateObservationsAllEqual(observations))
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

