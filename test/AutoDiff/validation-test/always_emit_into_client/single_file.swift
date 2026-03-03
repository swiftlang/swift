// REQUIRES: executable_test
// RUN: %empty-directory(%t)

/// Note: we build just a module without a library since it would not contain any exported
/// symbols because all the functions in the module are marked as @_alwaysEmitIntoClient.
// RUN: %target-build-swift %S/Inputs/SingleFileModule/file.swift -emit-module \
// RUN:   -emit-module-path %t/SingleFileModule.swiftmodule -module-name SingleFileModule

// RUN: %target-build-swift -I%t %s -o %t/a.out %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -I%t %s -emit-ir | %FileCheck %s

import SingleFileModule
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  expectEqual(42, gradient(at: 0, of: f))
  expectEqual(42, gradient(at: 1, of: f))
  expectEqual(42, gradient(at: 2, of: f))
}

runAllTests()

// CHECK: @"16SingleFileModule1fyS2fFWJrSpSr" = weak_odr hidden {{()|local_unnamed_addr }}global { ptr, ptr } { ptr @"$s16SingleFileModule1fyS2fFTJfSpSr", ptr @"$s16SingleFileModule1fyS2fFTJrSpSr" }
