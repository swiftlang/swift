// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import StdlibUnittest

_setTestSuiteFailedCallback() { print("abort()") }

struct RaceTest1 : RaceTestWithPerTrialDataType {
  static var shouldPass: Bool = true
  static var iterationCountdown: _stdlib_AtomicInt = _stdlib_AtomicInt(8)

  class RaceData {
    init() {}
  }

  typealias ThreadLocalData = Void
  typealias Observation = Observation1UWord

  func makeRaceData() -> RaceData {
    return RaceData()
  }

  func makeThreadLocalData() -> Void {
    return Void()
  }

  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData
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

  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S) {
    for observation in observations {
      switch observation {
      case Observation(0x1):
        sink.put(.Pass)
      case Observation(0x2):
        sink.put(.PassInteresting(String(observation)))
      case Observation(0xffff):
        sink.put(.Failure)
      case Observation(0xfffe):
        sink.put(.FailureInteresting(String(observation)))
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
// CHECK: out>>> Pass: {{.*}} times
// CHECK: out>>> Pass (2): 4 times
// CHECK: out>>> Failure: 0 times
// CHECK: [       OK ] Race.passes

RaceTestSuite.test("fails") {
  RaceTest1.shouldPass = false
  RaceTest1.iterationCountdown = _stdlib_AtomicInt(8)
  runRaceTest(RaceTest1.self, trials: 10)
}
// CHECK: [ RUN      ] Race.fails
// REQUIRES: executable_test
// CHECK: out>>> Pass: {{.*}} times
// CHECK: out>>> Pass (2): 1 times
// CHECK: out>>> Failure: 1 times
// CHECK: out>>> Failure (65534): 3 times
// CHECK: [     FAIL ] Race.fails
// CHECK: Race: Some tests failed, aborting

runAllTests()

