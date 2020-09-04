// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import NonMemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("plus (+)") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

OperatorsTestSuite.test("minus (-)") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs - rhs

  expectEqual(19, result.value)
}

OperatorsTestSuite.test("star (*)") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs * rhs

  expectEqual(966, result.value)
}

OperatorsTestSuite.test("slash (/)") {
  let lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs / rhs

  expectEqual(1, result.value)
}

OperatorsTestSuite.test("percent (%)") {
  let lhs = IntBox(value: 11)
  let rhs = IntBox(value: 2)

  let result = lhs % rhs

  expectEqual(1, result.value)
}

OperatorsTestSuite.test("amp (&)") {
  let lhs = IntBox(value: 6)
  let rhs = IntBox(value: 5)

  let result = lhs & rhs

  expectEqual(4, result.value)
}

OperatorsTestSuite.test("pipe (|)") {
  let lhs = IntBox(value: 6)
  let rhs = IntBox(value: 5)

  let result = lhs | rhs

  expectEqual(7, result.value)
}

OperatorsTestSuite.test("less (<)") {
  let lhs = IntBox(value: 5)
  let rhs = IntBox(value: 6)

  let result = lhs < rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("greater (>)") {
  let lhs = IntBox(value: 5)
  let rhs = IntBox(value: 6)

  let result = lhs > rhs

  expectEqual(false, result)
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

OperatorsTestSuite.test("equal equal (==)") {
  let lhs = IntBox(value: 5)
  let rhs = IntBox(value: 5)

  let result = lhs == rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("exclaim equal (!=)") {
  let lhs = IntBox(value: 5)
  let rhs = IntBox(value: 5)

  let result = lhs != rhs

  expectEqual(false, result)
}

OperatorsTestSuite.test("less equal (<=)") {
  let lhs = IntBox(value: 5)
  let rhs = IntBox(value: 5)

  let result = lhs <= rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("greater equal (>=)") {
  let lhs = IntBox(value: 6)
  let rhs = IntBox(value: 5)

  let result = lhs >= rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("amp amp (&&)") {
  let lhs = BoolBox(value: true)
  let rhs = BoolBox(value: false)

  let result = lhs && rhs

  expectEqual(false, result.value)
}

OperatorsTestSuite.test("pipe pipe (||)") {
  let lhs = BoolBox(value: true)
  let rhs = BoolBox(value: false)

  let result = lhs || rhs

  expectEqual(true, result.value)
}

runAllTests()
