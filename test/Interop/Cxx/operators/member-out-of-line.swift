// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/member-out-of-line.cpp -I %S/Inputs -o %t/member-out-of-line.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/member-out-of-line %t/member-out-of-line.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/member-out-of-line
// RUN: %target-run %t/member-out-of-line
//
// REQUIRES: executable_test
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberOutOfLine
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("LoadableIntWrapper.plus (out-of-line)") {
  var lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

OperatorsTestSuite.test("LoadableIntWrapper.call (out-of-line)") {
  var wrapper = LoadableIntWrapper(value: 42)

  let resultNoArgs = wrapper()
  let resultOneArg = wrapper(23)
  let resultTwoArgs = wrapper(3, 5)

  expectEqual(42, resultNoArgs)
  expectEqual(65, resultOneArg)
  expectEqual(57, resultTwoArgs)
}

OperatorsTestSuite.test("AddressOnlyIntWrapper.call (out-of-line)") {
  var wrapper = AddressOnlyIntWrapper(42)

  let resultNoArgs = wrapper()
  let resultOneArg = wrapper(23)
  let resultTwoArgs = wrapper(3, 5)

  expectEqual(42, resultNoArgs)
  expectEqual(65, resultOneArg)
  expectEqual(57, resultTwoArgs)
}

runAllTests()
