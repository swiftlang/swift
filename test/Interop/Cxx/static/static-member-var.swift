// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/static-member-var.cpp -I %S/Inputs -o %t/static-member-var.o -std=c++11
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-member-var.o -Xfrontend -enable-cxx-interop -Xcc -std=c++11
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticMemberVar
import StdlibUnittest

var StaticMemberVarTestSuite = TestSuite("StaticMemberVarTestSuite")

StaticMemberVarTestSuite.test("read-static-member-address") {
  expectEqual(
    &WithStaticMember.staticMember,
    WithStaticMember.getStaticMemberAddress())
}

StaticMemberVarTestSuite.test("write-static-member-from-cxx") {
  expectNotEqual(84, WithStaticMember.staticMember)
  WithStaticMember.setStaticMemberFromCxx(84)
  expectEqual(84, WithStaticMember.staticMember)
}

StaticMemberVarTestSuite.test("write-static-member-from-swift") {
  expectNotEqual(24, WithStaticMember.staticMember)
  WithStaticMember.staticMember = 24
  expectEqual(24, WithStaticMember.getStaticMemberFromCxx())
}

StaticMemberVarTestSuite.test("incomplete-array-static-member") {
  //TODO recognize array member variable `arrayMember`.
  // expectEqual(18, WithIncompleteStaticMember.arrayMember[0])
  // expectEqual(3, WithIncompleteStaticMember.arrayMember.count)
}

StaticMemberVarTestSuite.test("incomplete-self-static-member-address") {
  expectEqual(
    WithIncompleteStaticMember.getStaticMemberFromCxx()!,
    &WithIncompleteStaticMember.selfMember)
}

StaticMemberVarTestSuite.test("write-incomplete-self-static-member-from-cxx") {
  expectNotEqual(128, WithIncompleteStaticMember.selfMember.id)
  var newVal = WithIncompleteStaticMember()
  newVal.id = 128
  WithIncompleteStaticMember.setStaticMemberFromCxx(newVal)
  expectEqual(128, WithIncompleteStaticMember.selfMember.id)
}

StaticMemberVarTestSuite.test("write-incomplete-self-static-member-from-swift") {
  expectNotEqual(132, WithIncompleteStaticMember.selfMember.id)
  WithIncompleteStaticMember.selfMember.id = 132
  expectEqual(132, WithIncompleteStaticMember.getStaticMemberFromCxx()!.pointee.id)
}

StaticMemberVarTestSuite.test("const-static-member") {
  //TODO fix undefined reference to `WithConstStaticMember::notDefined`.
  // expectEqual(24, WithConstStaticMember.notDefined)
  expectEqual(48, WithConstStaticMember.defined)
  expectEqual(96, WithConstStaticMember.definedOutOfLine)
}

StaticMemberVarTestSuite.test("const-static-member") {
  //TODO fix undefined reference to `WithConstStaticMember::notDefined`.
  // expectEqual(24, WithConstStaticMember.notDefined)
  expectEqual(48, WithConstStaticMember.defined)
  expectEqual(96, WithConstStaticMember.definedOutOfLine)
}

StaticMemberVarTestSuite.test("const-static-member") {
  //TODO fix undefined reference to `WithConstStaticMember::notDefined`.
  // expectEqual(24, WithConstStaticMember.notDefined)
  expectEqual(48, WithConstStaticMember.defined)
  expectEqual(96, WithConstStaticMember.definedOutOfLine)
}

StaticMemberVarTestSuite.test("const-static-member") {
  //TODO fix undefined reference to `WithConstStaticMember::notDefined`.
  // expectEqual(WithConstStaticMember.notDefined)
  expectEqual(48, WithConstStaticMember.defined)
  expectEqual(96, WithConstStaticMember.definedOutOfLine)
}

// Check that variables with identical names in different namespaces don't
// collide in any intermediate representation of the compiler.
StaticMemberVarTestSuite.test("no-collisions") {
  expectEqual(144, ClassA.notUniqueName)
  expectEqual(169, ClassB.notUniqueName)
}

runAllTests()
