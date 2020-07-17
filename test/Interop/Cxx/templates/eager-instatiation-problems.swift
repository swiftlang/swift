// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import EagerInstantiationProblems
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("eager-instantiation-of-members") {
  // This will fail with:
  //
  // error: type 'int' cannot be used prior to '::' because it has no members
  //   T::getIntDoesNotExist();
  //
  // whereas in C++ this compiles.
  // TODO(scentini): Fix this
  // let _brokenMemberMagicWrapper = BrokenMemberMagicWrapper()
}

runAllTests()
