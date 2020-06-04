// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import NonMemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("plus") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

OperatorsTestSuite.test("minus") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs - rhs

  expectEqual(19, result.value)
}

OperatorsTestSuite.test("star") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs * rhs

  expectEqual(966, result.value)
}

OperatorsTestSuite.test("slash") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs / rhs

  expectEqual(1, result.value)
}

runAllTests()
