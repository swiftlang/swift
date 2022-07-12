// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)

// REQUIRES: executable_test

// This test verifies that Swift correctly handles calls to C++ functions that
// had Named Return Value Optimization applied to them.

import StdlibUnittest
import ReturnsLargeClass

var LargeTypes = TestSuite("Large C++ Return Types")

LargeTypes.test("NRVO") {
  let x = funcReturnsLargeClass()

  expectEqual(0, x.a1)
  expectEqual(2, x.a2)
  expectEqual(6, x.a6)
  expectEqual(0, x.a7)
}

runAllTests()
