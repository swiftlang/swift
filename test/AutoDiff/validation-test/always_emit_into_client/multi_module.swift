// REQUIRES: executable_test
// RUN: %empty-directory(%t)

/// Note: we build just modules without libraries since they would not contain any exported
/// symbols because all the functions in the module are marked as @_alwaysEmitIntoClient.
// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModule1)) %S/Inputs/MultiModule/file1.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModule1.swiftmodule -module-name MultiModule1
// RUN: %target-codesign %t/%target-library-name(MultiModule1)
// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModule2)) %S/Inputs/MultiModule/file2.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModule2.swiftmodule -module-name MultiModule2 -I%t %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(MultiModule2)

// RUN: %target-build-swift -I%t %s -o %t/a.out %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(MultiModule1) %t/%target-library-name(MultiModule2)

// RUN: %target-build-swift -I%t %s -emit-ir | %FileCheck %s

import MultiModule1
import MultiModule2
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  expectEqual(42, gradient(at: 0, of: f))
  expectEqual(42, gradient(at: 1, of: f))
  expectEqual(42, gradient(at: 2, of: f))
}

runAllTests()

// CHECK: @"12MultiModule11fyS2fFWJrSpSr" = weak_odr hidden {{()|local_unnamed_addr }}global { ptr, ptr } { ptr @"$s12MultiModule11fyS2fFTJfSpSr", ptr @"$s12MultiModule11fyS2fFTJrSpSr" }
