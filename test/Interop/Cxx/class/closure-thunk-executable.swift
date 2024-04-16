// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

// The test is enabled only on windows until https://github.com/apple/swift/pull/73019
// is fixed.
// REQUIRES: OS=windows-msvc

import StdlibUnittest
import Closure

var ClosureTestSuite = TestSuite("Closure")

ClosureTestSuite.test("ConvertToFunctionPointer") {
  cfunc2({N in})
}

runAllTests()
