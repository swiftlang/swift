// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | %FileCheck %s

import StdlibUnittest


_setTestSuiteFailedCallback() { print("abort()") }

struct RaceTest1 : RaceTestWithPerTrialData {
  static var shouldPass: Bool = true
  static var iterationCountdown: _stdlib_AtomicInt = _stdlib_AtomicInt(8)

  class RaceData {
    init() {}
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UInt

  func makeRaceData() -> RaceData {
    return RaceData()
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    _ raceData: RaceData, _ threadLocalData: inout ThreadLocalData
  ) -> Observation {
    switch RaceTest1.iterationCountdown.fetchAndAdd(-1) {
    case 0:
      return Observation(0x1)

    case 1:
      return Observation(RaceTest1.shouldPass ? 0x1 : 0xffff)

    case 1, 2:
      return Observation(0x2)

    case 3, 4, 5:
      return Observation(RaceTest1.shouldPass ? 0x2 : 0xfffe)

    default:
      return Observation(0x1)
    }
  }

  func evaluateObservations(
    _ observations: [Observation],
    _ sink: (RaceTestObservationEvaluation) -> Void
  ) {
    for observation in observations {
      switch observation {
      case Observation(0x1):
        sink(.pass)
      case Observation(0x2):
        sink(.passInteresting(String(describing: observation)))
      case Observation(0xffff):
        sink(.failure)
      case Observation(0xfffe):
        sink(.failureInteresting(String(describing: observation)))
      default:
        fatalError("should not happen")
      }
    }
  }
}

var RaceTestSuite = TestSuite("Race")

RaceTestSuite.test("passes") {
  RaceTest1.shouldPass = true
  RaceTest1.iterationCountdown = _stdlib_AtomicInt(8)
  runRaceTest(RaceTest1.self, trials: 10)
}
// CHECK: [ RUN      ] Race.passes
// CHECK: stdout>>> Pass: {{.*}} times
// CHECK: stdout>>> Pass (2): 4 times
// CHECK: stdout>>> Failure: 0 times
// CHECK: [       OK ] Race.passes

RaceTestSuite.test("fails") {
  RaceTest1.shouldPass = false
  RaceTest1.iterationCountdown = _stdlib_AtomicInt(8)
  runRaceTest(RaceTest1.self, trials: 10)
}
// CHECK: [ RUN      ] Race.fails
// REQUIRES: executable_test
// CHECK: stdout>>> Pass: {{.*}} times
// CHECK: stdout>>> Pass (2): 1 times
// CHECK: stdout>>> Failure: 1 times
// CHECK: stdout>>> Failure (65534): 3 times
// CHECK: [     FAIL ] Race.fails

RaceTestSuite.test("closure") {
  let count = _stdlib_AtomicInt(0)
  runRaceTest(trials: 10) {
    _ = count.fetchAndAdd(1)
  }
  expectNotEqual(0, count.load())
}
// CHECK: [ RUN      ] Race.closure
// CHECK: [       OK ] Race.closure
// CHECK: Race: Some tests failed, aborting

runAllTests()

