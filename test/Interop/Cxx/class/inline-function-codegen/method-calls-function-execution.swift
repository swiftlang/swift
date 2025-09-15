// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -Xcc -std=c++20 -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import MethodCallsFunction
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("method calls function") {
  expectEqual(42, callMethod(41))
}

func doSomethingWith(_ s: Cell) { s.set_marked(true) }

MembersTestSuite.test("method sets bitfield") {
  let s = createCell()
  doSomethingWith(s)
}

runAllTests()
