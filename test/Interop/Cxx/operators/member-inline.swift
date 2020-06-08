// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import MemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("plus") {
  var lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

runAllTests()
