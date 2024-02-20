// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: stress_test
// UNSUPPORTED: threading_none

import StdlibUnittest

let iterations = 100

class Thing {}

class WBox<T: AnyObject> {
  weak var wref: T?
  init(_ ref: T) { self.wref = ref }
  init() { self.wref = nil }
}

class WeakReferenceRaceData {
  let closure: () -> Void
  init(_ closure: @escaping () -> Void) {
    self.closure = closure
  }
}

protocol WeakReferenceRaceTest: RaceTestWithPerTrialData {
  associatedtype RaceData = WeakReferenceRaceData
  associatedtype ThreadLocalData = Void
  associatedtype Observation = Observation1UInt
}

extension WeakReferenceRaceTest {
  func makeThreadLocalData() -> Void {
    return ()
  }

  func thread1(
    _ raceData: WeakReferenceRaceData,
    _ threadLocalData: inout Void
    ) -> Observation1UInt {
    raceData.closure()
    // The trial succeeds by completing without crashing
    return Observation1UInt(0)
  }

  func evaluateObservations(
    _ observations: [Observation1UInt],
    _ sink: (RaceTestObservationEvaluation) -> Void
    ) {
    sink(evaluateObservationsAllEqual(observations))
  }
}

let WeakReferenceRaceTests = TestSuite("WeakReferenceRaceTests")

struct RaceTest_instancePropertyCopy: WeakReferenceRaceTest {
  func makeRaceData() -> WeakReferenceRaceData {
    // Capture a weak reference via its container object.
    // https://github.com/apple/swift/issues/42814
    let box = WBox(Thing())
    return WeakReferenceRaceData {
      let nbox = WBox<Thing>()
      nbox.wref = box.wref
      _blackHole(nbox)
    }
  }
}

WeakReferenceRaceTests.test("class instance property (copy)") {
  runRaceTest(RaceTest_instancePropertyCopy.self, trials: iterations)
}

struct RaceTest_instancePropertyLoad: WeakReferenceRaceTest {
  func makeRaceData() -> WeakReferenceRaceData {
    // Capture a weak reference via its container object.
    // https://github.com/apple/swift/issues/42814
    let box = WBox(Thing())
    return WeakReferenceRaceData {
      if let ref = box.wref {
        _blackHole(ref)
      }
    }
  }
}

WeakReferenceRaceTests.test("class instance property (load)") {
  runRaceTest(RaceTest_instancePropertyLoad.self, trials: iterations)
}

struct RaceTest_directCaptureCopy: WeakReferenceRaceTest {
  func makeRaceData() -> WeakReferenceRaceData {
    weak var wref = Thing()
    return WeakReferenceRaceData {
      let nbox = WBox<Thing>()
      nbox.wref = wref
      _blackHole(nbox)
    }
  }
}

WeakReferenceRaceTests.test("direct capture (copy)") {
  runRaceTest(RaceTest_directCaptureCopy.self, trials: iterations)
}

struct RaceTest_directCaptureLoad: WeakReferenceRaceTest {
  func makeRaceData() -> WeakReferenceRaceData {
    weak var wref = Thing()
    return WeakReferenceRaceData {
      let nbox = WBox<Thing>()
      nbox.wref = wref
      _blackHole(nbox)
    }
  }
}

WeakReferenceRaceTests.test("direct capture (load)") {
  runRaceTest(RaceTest_directCaptureLoad.self, trials: iterations)
}

runAllTests()
