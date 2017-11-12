// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest


import CoreMedia

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
