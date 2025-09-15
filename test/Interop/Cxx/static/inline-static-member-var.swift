// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/inline-static-member-var.cpp -I %S/Inputs -o %t/inline-static-member-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/inline-static-member-var.o -Xfrontend -enable-experimental-cxx-interop
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics 2&>1
//
// REQUIRES: executable_test

import InlineStaticMemberVar
import StdlibUnittest

var InlineStaticMemberVarTestSuite = TestSuite("InlineStaticMemberVarTestSuite")

InlineStaticMemberVarTestSuite.test("read-inline-static-member-address") {
  expectEqual(
    &WithInlineStaticMember.staticMember,
    WithInlineStaticMember.getStaticMemberAddress())
}

InlineStaticMemberVarTestSuite.test("read-inline-static-member-init-at-runtime") {
  expectEqual(42, WithInlineStaticMember.staticMemberInitializedAtRuntime)
}

InlineStaticMemberVarTestSuite.test("write-inline-static-member-from-cxx") {
  expectNotEqual(128, WithInlineStaticMember.staticMember)
  WithInlineStaticMember.setStaticMemberFromCxx(128)
  expectEqual(128, WithInlineStaticMember.staticMember)
}

InlineStaticMemberVarTestSuite.test("write-inline-static-member-from-swift") {
  expectNotEqual(256, WithInlineStaticMember.staticMember)
  WithInlineStaticMember.staticMember = 256
  expectEqual(256, WithInlineStaticMember.getStaticMemberFromCxx())
}

runAllTests()
