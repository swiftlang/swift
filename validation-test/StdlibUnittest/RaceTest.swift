// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | %FileCheck %s
// UNSUPPORTED: nonatomic_rc

import StdlibUnittest
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
import Glibc
#endif


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

RaceTestSuite.test("timeout-zero") {
  // Zero timeout is still expected to run at least one trial.
  let count = _stdlib_AtomicInt(0)
  runRaceTest(trials: 2000000000, timeoutInSeconds: 0) {
    _ = count.fetchAndAdd(1)
  }
  expectGT(count.load(), 0)
}
// CHECK: [ RUN      ] Race.timeout-zero
// CHECK: [       OK ] Race.timeout-zero


func -(_ lhs: timeval, _ rhs: timeval) -> timeval {
  var result = timeval(tv_sec: 0, tv_usec: 0)
  result.tv_sec = lhs.tv_sec - rhs.tv_sec
  result.tv_usec = lhs.tv_usec - rhs.tv_usec
  if result.tv_usec < 0 {
    result.tv_usec += 1000000
    result.tv_sec -= 1
  }
  return result
}

func gettimeofday() -> timeval {
  var result = timeval(tv_sec: 0, tv_usec: 0)
  gettimeofday(&result, nil)
  return result
}

RaceTestSuite.test("timeout-small") {
  // Verify that the timeout fires after the correct number of seconds.
  // If the timeout fails to fire then this test will run for a very long time.
  var startTime: timeval
  var endTime: timeval
  let timeout = 5
  let count = _stdlib_AtomicInt(0)
  startTime = gettimeofday()
  runRaceTest(trials: 2000000000, timeoutInSeconds: timeout) {
    _ = count.fetchAndAdd(1)
  }
  endTime = gettimeofday()
  expectGT(count.load(), 0)
  // Test should have run to the timeout.
  // Test should not have run too long after the timeout.
  let duration = endTime - startTime
  expectGE(duration.tv_sec, timeout)
  expectLT(duration.tv_sec, timeout*100)  // large to avoid spurious failures
}
// CHECK: [ RUN      ] Race.timeout-small
// CHECK: [       OK ] Race.timeout-small

RaceTestSuite.test("timeout-big") {
  // Verify that a short test with a long timeout completes before the timeout.
  var startTime: timeval
  var endTime: timeval
  let timeout = 10000
  let count = _stdlib_AtomicInt(0)
  startTime = gettimeofday()
  runRaceTest(trials: 10, timeoutInSeconds: timeout) {
    _ = count.fetchAndAdd(1)
  }
  endTime = gettimeofday()
  expectGT(count.load(), 0)
  // Test should have stopped long before the timeout.
  let duration = endTime - startTime
  expectLT(duration.tv_sec, timeout / 2)
}
// CHECK: [ RUN      ] Race.timeout-big
// CHECK: [       OK ] Race.timeout-big

runAllTests()
// CHECK: Race: Some tests failed, aborting

