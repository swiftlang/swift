// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// This file has CRT-specific C stdlib tests, that use
// some APIs present only in CRT.

import StdlibUnittest
import CRT

var CRTTests = TestSuite("CRT")

CRTTests.test("complex functions available in Swift") {
  let complexValue = _Cbuild(1.0, 2.0) // Construct a complex double using MSVC-specific API.
  let re = creal(complexValue)
  expectEqual(re, 1.0)
}
