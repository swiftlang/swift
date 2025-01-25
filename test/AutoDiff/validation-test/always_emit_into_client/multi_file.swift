// REQUIRES: executable_test
// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiFileModule)) %S/Inputs/MultiFileModule/file1.swift %S/Inputs/MultiFileModule/file2.swift \
// RUN:   -emit-module -emit-module-path %t/MultiFileModule.swiftmodule -module-name MultiFileModule
// RUN: %target-build-swift -I%t -L%t %s -lm -lMultiFileModule -o %t/a.out %target-rpath(%t)
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -I%t %s -emit-ir | %FileCheck %s

import MultiFileModule
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  expectEqual(42, gradient(at: 0, of: f))
  expectEqual(42, gradient(at: 1, of: f))
  expectEqual(42, gradient(at: 2, of: f))
}

runAllTests()

// CHECK: @"15MultiFileModule1fyS2fFWJrSpSr" = weak_odr hidden global { ptr, ptr } { ptr @"$s15MultiFileModule1fyS2fFTJfSpSr", ptr @"$s15MultiFileModule1fyS2fFTJrSpSr" }, align 8
