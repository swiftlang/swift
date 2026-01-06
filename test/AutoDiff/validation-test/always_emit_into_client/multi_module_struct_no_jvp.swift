// REQUIRES: executable_test
// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModuleStruct1)) %S/Inputs/MultiModuleStruct/file1.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleStruct1.swiftmodule -module-name MultiModuleStruct1
// RUN: %target-codesign %t/%target-library-name(MultiModuleStruct1)
// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModuleStruct2NoJVP)) %S/Inputs/MultiModuleStruct/file2_no_jvp.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleStruct2NoJVP.swiftmodule -module-name MultiModuleStruct2NoJVP -I%t -L%t -lMultiModuleStruct1 %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(MultiModuleStruct2NoJVP)

/// Note: we enable forward-mode differentiation to automatically generate JVP for `foo`.
/// It wraps `Struct.sum` that has custom JVP defined in MultiModuleStruct2, so we can test it.
// RUN: %target-build-swift -Xfrontend -enable-experimental-forward-mode-differentiation \
// RUN:   -I%t -L%t %s -lMultiModuleStruct1 -lMultiModuleStruct2NoJVP -o %t/a.out %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(MultiModuleStruct1) %t/%target-library-name(MultiModuleStruct2NoJVP)

// RUN: %target-build-swift -I%t %s -emit-ir | %FileCheck %s

import MultiModuleStruct1
import MultiModuleStruct2NoJVP
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  func foo(x: Struct) -> Float { x.sum() }
  expectEqual(Struct(42), pullback(at: Struct(0), of: foo)(1))
  expectEqual(Struct(42), pullback(at: Struct(1), of: foo)(1))
  expectEqual(Struct(42), pullback(at: Struct(2), of: foo)(1))
  /// Custom JVP for Struct.sum is not provided, a JVP causing fatal error is emitted.
  expectCrash{differential(at: Struct(0), of: foo)(Struct(1))}
  expectCrash{differential(at: Struct(1), of: foo)(Struct(1))}
  expectCrash{differential(at: Struct(2), of: foo)(Struct(1))}
}

runAllTests()

// CHECK: @"18MultiModuleStruct16StructV3sumSfyFWJrSpSr" = weak_odr hidden {{()|local_unnamed_addr }}global { ptr, ptr } { ptr @"$s18MultiModuleStruct16StructV3sumSfyFTJfSpSr", ptr @"$s18MultiModuleStruct16StructV3sumSfyFTJrSpSr" }
