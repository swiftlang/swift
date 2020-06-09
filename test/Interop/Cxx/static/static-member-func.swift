// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/static-member-func.cpp -I %S/Inputs -o %t/static-member-func.o -std=c++11
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-member-func.o -Xfrontend -enable-cxx-interop -Xcc -std=c++11
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticMemberFunc
import StdlibUnittest

var StaticMemberFuncTestSuite = TestSuite("StaticMemberFuncTestSuite")

StaticMemberFuncTestSuite.test("call-static-member-func") {
  expectEqual(
    1234,
    WithStaticMemberFunc.staticMemberFunc())
}

runAllTests()
