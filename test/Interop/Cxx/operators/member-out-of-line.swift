// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/member-out-of-line.cpp -I %S/Inputs -o %t/member-out-of-line.o -std=c++17
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

OperatorsTestSuite.test("plus") {
  var lhs = IntBox(value: 42)
  let rhs = IntBox(value: 23)

  let result = lhs + rhs

  expectEqual(65, result.value)
}

runAllTests()
