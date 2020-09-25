// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberInline
import StdlibUnittest

var OperatorsTestSuite = TestSuite("Operators")

OperatorsTestSuite.test("LoadableIntWrapper.plus") {
  var lhs = LoadableIntWrapper(value: 42)
  let rhs = LoadableIntWrapper(value: 23)

  let result = lhs - rhs

  expectEqual(19, result.value)
}

runAllTests()
