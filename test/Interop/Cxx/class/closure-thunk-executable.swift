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

func takeNonTrivial(_ n: NonTrivial) {}
@available(SwiftStdlib 5.8, *)
func takeSharedRef(_ x: SharedRef) {}

ClosureTestSuite.test("ConvertNamedFunctionToFunctionPointer") {
  cfunc2(takeNonTrivial)
}

ClosureTestSuite.test("Pass FRT to named function pointer") {
  if #available(SwiftStdlib 5.8, *) {
    cppGo(takeSharedRef)
  }
}

runAllTests()
