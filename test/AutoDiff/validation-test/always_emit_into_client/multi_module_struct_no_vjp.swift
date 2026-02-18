// REQUIRES: executable_test
// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModuleStruct1)) %S/Inputs/MultiModuleStruct/file1.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleStruct1.swiftmodule -module-name MultiModuleStruct1
// RUN: not %target-build-swift-dylib(%t/%target-library-name(MultiModuleStruct2NoVJP)) %S/Inputs/MultiModuleStruct/file2_no_vjp.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleStruct2NoVJP.swiftmodule -module-name MultiModuleStruct2NoVJP -I%t -L%t -lMultiModuleStruct1 %target-rpath(%t) 2>&1 | \
// RUN:   %FileCheck %s

// CHECK: file2_no_vjp.swift:6:4: error: function is not differentiable

import MultiModuleStruct1
import MultiModuleStruct2
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  func foo(x: Struct) -> Float { x.sum() }
  expectEqual(42, differential(at: Struct(0), of: foo)(Struct(1)))
  expectEqual(42, differential(at: Struct(1), of: foo)(Struct(1)))
  expectEqual(42, differential(at: Struct(2), of: foo)(Struct(1)))
}

runAllTests()
