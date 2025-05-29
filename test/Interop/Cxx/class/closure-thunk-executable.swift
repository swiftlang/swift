// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import Closure

var ClosureTestSuite = TestSuite("Closure")

ClosureTestSuite.test("ConvertToFunctionPointer") {
  cfunc2({N in})
}

ClosureTestSuite.test("Pass FRT to function pointer") {
  cppGo({N in })
}

runAllTests()
