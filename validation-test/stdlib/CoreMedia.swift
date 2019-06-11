// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

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

CoreMediaTests.test("CMTime(seconds:preferredTimescale:)") {
  let time = CMTime(seconds: 7.0, preferredTimescale: 600)
  expectTrue(time.isValid)
  expectEqual(7.0, time.seconds)
}

CoreMediaTests.test("CMTime/Comparable") {
  let instances: [(Int, CMTime)] = [
    (-9999, CMTime(seconds: -Double.infinity, preferredTimescale: 600)),
    (10, CMTime(seconds: 10.0, preferredTimescale: 600)),
    (10, CMTime(seconds: 10.0, preferredTimescale: 600)),
    (20, CMTime(seconds: 20.0, preferredTimescale: 600)),
    (30, CMTime(seconds: 30.0, preferredTimescale: 600)),
    (9999, CMTime(seconds: Double.infinity, preferredTimescale: 600)),
  ]
  func comparisonOracle(i: Int, j: Int) -> ExpectedComparisonResult {
    return instances[i].0 <=> instances[j].0
  }
  checkComparable(instances.map { $0.1 }, oracle: comparisonOracle)
}

CoreMediaTests.test("CMTimeRange(start:duration:)") {
  let start = CMTime(seconds: 10.0, preferredTimescale: 600)
  let duration = CMTime(seconds: 5.0, preferredTimescale: 600)
  let range = CMTimeRange(start: start, duration: duration)
  expectEqual(start, range.start)
  expectEqual(duration, range.duration)
}

CoreMediaTests.test("CMTimeRange(start:end:)") {
  let start = CMTime(seconds: 10.0, preferredTimescale: 600)
  let end = CMTime(seconds: 20.0, preferredTimescale: 600)
  let range = CMTimeRange(start: start, end: end)
  expectEqual(start, range.start)
  expectEqual(end, range.end)
}

CoreMediaTests.test("CMTimeRange/Equatable") {
  let time10 = CMTime(seconds: 10.0, preferredTimescale: 600)
  let time20 = CMTime(seconds: 20.0, preferredTimescale: 600)
  let timeInf = CMTime(seconds: Double.infinity, preferredTimescale: 600)

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


var CMTimeTests = TestSuite("CMTime")

let t1 = CMTimeMake(1, 10)
let t2 = CMTimeMake(2, 10)

CMTimeTests.test("isValid") {
  expectFalse(kCMTimeInvalid.isValid)
  expectTrue(t1.isValid)
}

CMTimeTests.test("isPositiveInfinity") {
  expectTrue(kCMTimePositiveInfinity.isPositiveInfinity)
  expectFalse(t1.isPositiveInfinity)
}

CMTimeTests.test("isNegativeInfinity") {
  expectTrue(kCMTimeNegativeInfinity.isNegativeInfinity)
  expectFalse(t1.isNegativeInfinity)
}

CMTimeTests.test("isIndefinite") {
  expectTrue(kCMTimeIndefinite.isIndefinite)
  expectFalse(t1.isIndefinite)
}

CMTimeTests.test("isNumeric") {
  expectFalse(kCMTimeInvalid.isNumeric)
  expectFalse(kCMTimePositiveInfinity.isNumeric)
  expectFalse(kCMTimeNegativeInfinity.isNumeric)
  expectFalse(kCMTimeIndefinite.isNumeric)
  expectTrue(t1.isNumeric)

  expectFalse(t1.hasBeenRounded)

  expectEqual(0.1, t1.seconds)

  t1.convertScale(100, method: CMTimeRoundingMethod.`default`)

  var t1a = t1.convertScale(11, method: CMTimeRoundingMethod.`default`)
  expectTrue(t1a.hasBeenRounded)
}

CMTimeTests.test("operators") {
  expectEqual(CMTimeMake(3, 10), t1 + t2)
  expectEqual(CMTimeMake(1, 10), t2 - t1)

  expectFalse(t1 < kCMTimeZero)
  expectFalse(t1 == kCMTimeZero)
  expectTrue(t1 > kCMTimeZero)

  expectFalse(t1 <= kCMTimeZero)
  expectTrue(t1 >= kCMTimeZero)
  expectTrue(t1 != kCMTimeZero)
}

let r1 = CMTimeRangeMake(kCMTimeZero, CMTimeMake(1, 1))
let r2 = CMTimeRangeMake(kCMTimeZero, kCMTimeZero)
let r3 = CMTimeRangeMake(CMTimeMake(1, 1), CMTimeMake(3, 2))

var CMTimeRangeTests = TestSuite("CMTimeRange")

CMTimeRangeTests.test("isValid") {
  expectTrue(r1.isValid)
  expectTrue(r2.isValid)
  expectTrue(r3.isValid)
  expectFalse(kCMTimeRangeInvalid.isValid)
}

CMTimeRangeTests.test("isIndefinite") {
  expectFalse(r1.isIndefinite)
  expectFalse(r2.isIndefinite)
  expectFalse(r3.isIndefinite)
}

CMTimeRangeTests.test("isEmpty") {
  expectFalse(r1.isEmpty)
  expectTrue(r2.isEmpty)
  expectFalse(r3.isEmpty)
}

CMTimeRangeTests.test("start/end") {
  expectEqual(CMTimeMake(1, 1), r3.start)
  expectEqual(CMTimeMake(5, 2), r3.end)
}

CMTimeRangeTests.test("union") {
  expectEqual(r1, r1.union(r2))
  expectEqual(r2, r2.union(r2))
  expectNotEqual(r3, r2.union(r3))
}

CMTimeRangeTests.test("intersection") {
  expectEqual(r2, r1.intersection(r2))
  expectEqual(r2, r1.intersection(r3))
}

CMTimeRangeTests.test("contains") {
  expectTrue(r1.containsTimeRange(r2))
  expectFalse(r2.containsTimeRange(r2))
  expectFalse(r2.containsTimeRange(r1))
  expectFalse(r3.containsTime(kCMTimeZero))
}

CMTimeRangeTests.test("operators") {
  expectFalse(r1 == r2)
  expectTrue(r1 != r2)
}

runAllTests()

