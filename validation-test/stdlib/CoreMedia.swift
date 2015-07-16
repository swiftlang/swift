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

CoreMediaTests.test("init(value:timescale)") {
  let time = CMTime(value: 7, timescale: 1)
  expectTrue(time.isValid)
  expectEqual(7, time.value)
  expectEqual(1, time.timescale)
}

CoreMediaTests.test("init(seconds:preferredTimeScale)") {
  let time = CMTime(seconds: 7.0, preferredTimeScale: 600)
  expectTrue(time.isValid)
  expectEqual(7.0, time.seconds)
}

runAllTests()

