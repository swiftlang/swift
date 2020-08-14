// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/static-member-var.cpp -I %S/Inputs -o %t/static-member-var.o -std=c++17
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-member-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticMemberVar
import StdlibUnittest

var ConstexprStaticMemberVarTestSuite = TestSuite("ConstexprStaticMemberVarTestSuite")

ConstexprStaticMemberVarTestSuite.test("constexpr-static-member") {
  expectEqual(139, WithConstexprStaticMember.definedInline)
}

runAllTests()
