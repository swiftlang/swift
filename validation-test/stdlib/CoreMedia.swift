// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest
import CoreMedia

var CoreMediaTests = TestSuite("CoreMedia")

// This is a basic smoke test to check that the overlay works.  It should be
// replaced with comprehensive tests.
CoreMediaTests.test("OverlaySmokeTest") {
  expectTrue(CMTimeMake(1, 10).isValid)
  expectTrue(CMTimeRangeMake(CMTimeMake(1, 10), CMTimeMake(2, 20)).isValid)
}

CoreMediaTests.test("CMTime(value:timescale:)") {
  let time = CMTime(value: 7, timescale: 1)
  expectTrue(time.isValid)
  expectEqual(7, time.value)
  expectEqual(1, time.timescale)
}

CoreMediaTests.test("CMTime(seconds:preferredTimeScale:)") {
  let time = CMTime(seconds: 7.0, preferredTimeScale: 600)
  expectTrue(time.isValid)
  expectEqual(7.0, time.seconds)
}

CoreMediaTests.test("CMTime/Comparable") {
  let instances: [(Int, CMTime)] = [
    (-9999, CMTime(seconds: -Double.infinity, preferredTimeScale: 600)),
    (10, CMTime(seconds: 10.0, preferredTimeScale: 600)),
    (10, CMTime(seconds: 10.0, preferredTimeScale: 600)),
    (20, CMTime(seconds: 20.0, preferredTimeScale: 600)),
    (30, CMTime(seconds: 30.0, preferredTimeScale: 600)),
    (9999, CMTime(seconds: Double.infinity, preferredTimeScale: 600)),
  ]
  func comparisonOracle(i: Int, j: Int) -> ExpectedComparisonResult {
    return instances[i].0 <=> instances[j].0
  }
  checkComparable(instances.map { $0.1 }, oracle: comparisonOracle)
}

CoreMediaTests.test("CMTimeRange(start:end:)") {
  let start = CMTime(seconds: 10.0, preferredTimeScale: 600)
  let end = CMTime(seconds: 20.0, preferredTimeScale: 600)
  let range = CMTimeRange(start: start, end: end)
  expectEqual(start, range.start)
  expectEqual(end, range.end)
}

CoreMediaTests.test("CMTimeRange/Equatable") {
  let time10 = CMTime(seconds: 10.0, preferredTimeScale: 600)
  let time20 = CMTime(seconds: 20.0, preferredTimeScale: 600)
  let timeInf = CMTime(seconds: Double.infinity, preferredTimeScale: 600)

  let instances: [(Int, CMTimeRange)] = [
    (10, CMTimeRangeMake(time10, time10)),
    (10, CMTimeRangeMake(time10, time10)),

    (20, CMTimeRangeMake(time10, time20)),
    (20, CMTimeRangeMake(time10, time20)),

    (9999, CMTimeRangeMake(timeInf, time20)),
    (9999, CMTimeRangeMake(timeInf, time20)),
  ]
  func comparisonOracle(i: Int, j: Int) -> Bool {
    return instances[i].0 == instances[j].0
  }
  checkEquatable(instances.map { $0.1 }, oracle: comparisonOracle)
}

runAllTests()

