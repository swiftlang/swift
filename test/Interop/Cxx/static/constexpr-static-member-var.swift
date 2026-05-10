// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/static-member-var.cpp -I %S/Inputs -o %t/static-member-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-member-var.o -Xfrontend -enable-experimental-cxx-interop
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticMemberVar
import StdlibUnittest

var ConstexprStaticMemberVarTestSuite = TestSuite("ConstexprStaticMemberVarTestSuite")

runAllTests()
