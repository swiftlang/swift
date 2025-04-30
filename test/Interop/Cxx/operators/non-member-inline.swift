// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import NonMemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("plus (+)") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

OperatorsTestSuite.test("minus (-)") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs - rhs

  expectEqual(19, result.value)
}

OperatorsTestSuite.test("star (*)") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs * rhs

  expectEqual(966, result.value)
}

OperatorsTestSuite.test("slash (/)") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs / rhs

  expectEqual(1, result.value)
}

OperatorsTestSuite.test("caret (^)") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs ^ rhs

  expectEqual(61, result.value)
}

OperatorsTestSuite.test("percent (%)") {
  let lhs = LoadableIntWrapper(value: 11)
  let rhs = LoadableIntWrapper(value: 2)

  let result = lhs % rhs

  expectEqual(1, result.value)
}

OperatorsTestSuite.test("amp (&)") {
  let lhs = LoadableIntWrapper(value: 6)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs & rhs

  expectEqual(4, result.value)
}

OperatorsTestSuite.test("pipe (|)") {
  let lhs = LoadableIntWrapper(value: 6)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs | rhs

  expectEqual(7, result.value)
}

OperatorsTestSuite.test("less (<)") {
  let lhs = LoadableIntWrapper(value: 5)
  let rhs = LoadableIntWrapper(value: 6)

  let result = lhs < rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("greater (>)") {
  let lhs = LoadableIntWrapper(value: 5)
  let rhs = LoadableIntWrapper(value: 6)

  let result = lhs > rhs

  expectEqual(false, result)
}

OperatorsTestSuite.test("less less (<<)") {
  let lhs = LoadableIntWrapper(value: 2)
  let rhs = LoadableIntWrapper(value: 4)

  let result = lhs << rhs

  expectEqual(32, result.value)
}

OperatorsTestSuite.test("greater greater (>>)") {
  let lhs = LoadableIntWrapper(value: 512)
  let rhs = LoadableIntWrapper(value: 8)

  let result = lhs >> rhs

  expectEqual(2, result.value)
}

OperatorsTestSuite.test("equal equal (==)") {
  let lhs = LoadableIntWrapper(value: 5)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs == rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("exclaim equal (!=)") {
  let lhs = LoadableIntWrapper(value: 5)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs != rhs

  expectEqual(false, result)
}

OperatorsTestSuite.test("less equal (<=)") {
  let lhs = LoadableIntWrapper(value: 5)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs <= rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("greater equal (>=)") {
  let lhs = LoadableIntWrapper(value: 6)
  let rhs = LoadableIntWrapper(value: 5)

  let result = lhs >= rhs

  expectEqual(true, result)
}

OperatorsTestSuite.test("slash equal (/=)") {
  var lhs = LoadableIntWrapper(value: 8)
  let rhs = LoadableIntWrapper(value: 2)

  lhs /= rhs

  expectEqual(lhs.value, 4)
}

OperatorsTestSuite.test("star equal (*=)") {
  var lhs = LoadableIntWrapper(value: 8)
  let rhs = LoadableIntWrapper(value: 2)

  lhs *= rhs

  expectEqual(lhs.value, 16)
}

OperatorsTestSuite.test("amp amp (&&)") {
  let lhs = LoadableBoolWrapper(value: true)
  let rhs = LoadableBoolWrapper(value: false)

  let result = lhs && rhs

  expectEqual(false, result.value)
}

OperatorsTestSuite.test("pipe pipe (||)") {
  let lhs = LoadableBoolWrapper(value: true)
  let rhs = LoadableBoolWrapper(value: false)

  let result = lhs || rhs

  expectEqual(true, result.value)
}

OperatorsTestSuite.test("UnnamedParameterInOperator.equal") {
  let lhs = ClassWithOperatorEqualsParamUnnamed()
  let rhs = ClassWithOperatorEqualsParamUnnamed()
  expectFalse(lhs == rhs)
}

OperatorsTestSuite.test("LValueAndRValueArithmetic.+") {
  let lhs = LValueAndRValueArithmetic(value: 123)
  let rhs = LValueAndRValueArithmetic(value: 146)

  expectEqual(269, (lhs + rhs).value)
}

runAllTests()
