// REQUIRES: executable_test
// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModuleProtocol1)) %S/Inputs/MultiModuleProtocol/file1.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleProtocol1.swiftmodule -module-name MultiModuleProtocol1
// RUN: %target-codesign %t/%target-library-name(MultiModuleProtocol1)

/// Note: we build just a module without a library since it would not contain any exported
/// symbols because all the functions in the module are marked as @_alwaysEmitIntoClient.
// RUN: %target-build-swift %S/Inputs/MultiModuleProtocol/file2.swift -emit-module -emit-module-path %t/MultiModuleProtocol2.swiftmodule \
// RUN:   -module-name MultiModuleProtocol2 -I%t -lMultiModuleProtocol1 %target-rpath(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(MultiModuleProtocol3)) %S/Inputs/MultiModuleProtocol/file3.swift \
// RUN:   -emit-module -emit-module-path %t/MultiModuleProtocol3.swiftmodule -module-name MultiModuleProtocol3 -I%t -L%t -lMultiModuleProtocol1 %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(MultiModuleProtocol3)

/// Note: we enable forward-mode differentiation to automatically generate JVP for `foo`.
/// It wraps `Protocol.sum` that has custom JVP defined in MultiModuleProtocol2, so we can test it.
// RUN: %target-build-swift -Xfrontend -enable-experimental-forward-mode-differentiation \
// RUN:   -I%t -L%t %s -lMultiModuleProtocol1 -lMultiModuleProtocol3 -o %t/a.out %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(MultiModuleProtocol1) %t/%target-library-name(MultiModuleProtocol3)

// RUN: %target-build-swift -I%t %s -emit-ir | %FileCheck %s

import MultiModuleProtocol1
import MultiModuleProtocol2
import MultiModuleProtocol3
import StdlibUnittest
import _Differentiation

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  func foo<T: Protocol>(x: T) -> Float
    where T: Differentiable, T.TangentVector == T { x.sum() }
  expectEqual(Struct(42), pullback(at: Struct(0), of: foo)(1))
  expectEqual(Struct(42), pullback(at: Struct(1), of: foo)(1))
  expectEqual(Struct(42), pullback(at: Struct(2), of: foo)(1))
  expectEqual(42, differential(at: Struct(0), of: foo)(Struct(1)))
  expectEqual(42, differential(at: Struct(1), of: foo)(Struct(1)))
  expectEqual(42, differential(at: Struct(2), of: foo)(Struct(1)))
}

runAllTests()

// CHECK: @"20MultiModuleProtocol18ProtocolPAAE3sumSfyFAaBRz16_Differentiation14DifferentiableRz13TangentVectorAeFPQzRszlWJrSpSr" = weak_odr hidden {{()|local_unnamed_addr }}global { ptr, ptr } { ptr @"$s20MultiModuleProtocol18ProtocolPAAE3sumSfyFAaBRz16_Differentiation14DifferentiableRz13TangentVectorAeFPQzRszlTJfSpSr", ptr @"$s20MultiModuleProtocol18ProtocolPAAE3sumSfyFAaBRz16_Differentiation14DifferentiableRz13TangentVectorAeFPQzRszlTJrSpSr" }
