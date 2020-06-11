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

OperatorsTestSuite.test("less less (<<)") {
  let lhs = IntBox(value: 2)
  let rhs = IntBox(value: 4)

  let result = lhs << rhs

  expectEqual(32, result.value)
}

OperatorsTestSuite.test("greater greater (>>)") {
  let lhs = IntBox(value: 512)
  let rhs = IntBox(value: 8)

  let result = lhs >> rhs

  expectEqual(2, result.value)
}

runAllTests()
