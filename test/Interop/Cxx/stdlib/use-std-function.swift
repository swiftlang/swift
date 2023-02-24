// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
//
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import StdFunction
import CxxStdlib
import Cxx

var StdFunctionTestSuite = TestSuite("StdFunction")

StdFunctionTestSuite.test("invokeWith42") {
    invokeWith42(Func { $0 + 1 }) // as a block

    let cClosure: @convention(c) (Int) -> Int = { $0 + 1 }
    invokeWith42(Func(cClosure)) // as a c function pointer
}

runAllTests()
