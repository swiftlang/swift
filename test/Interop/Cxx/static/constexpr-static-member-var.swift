// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/static-member-var.cpp -I %S/Inputs -o %t/static-member-var.o
// NOTE: we must use `-O` here to ensure that the constexpr value is inlined and no undefined reference remains.
// RUN: %target-build-swift -O %s -I %S/Inputs -o %t/statics %t/static-member-var.o -Xfrontend -enable-cxx-interop
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
