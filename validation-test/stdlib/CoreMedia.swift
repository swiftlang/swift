// RUN: %target-run-simple-swift

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

runAllTests()

