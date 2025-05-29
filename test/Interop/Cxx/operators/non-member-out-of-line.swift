// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/non-member-out-of-line.cpp -I %S/Inputs -o %t/non-member-out-of-line.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/non-member-out-of-line %t/non-member-out-of-line.o -Xfrontend -enable-experimental-cxx-interop
// RUN: %target-codesign %t/non-member-out-of-line
// RUN: %target-run %t/non-member-out-of-line
//
// REQUIRES: executable_test

import NonMemberOutOfLine
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("plus") {
  let lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

OperatorsTestSuite.test("UnnamedParameterInOperator.equal") {
  let lhs = ClassWithOperatorEqualsParamUnnamed()
  let rhs = ClassWithOperatorEqualsParamUnnamed()
  expectFalse(lhs == rhs)
  expectTrue(lhs != rhs)
}

runAllTests()
