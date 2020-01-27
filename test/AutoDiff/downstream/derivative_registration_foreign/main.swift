// RUN: %empty-directory(%t)
// RUN: %clang -c %S/Inputs/Foreign.c -fmodules -o %t/CForeign.o
// RUN: %target-swift-emit-silgen -enable-experimental-cross-file-derivative-registration -I %S/Inputs -I %t %s | %FileCheck %s --check-prefix=CHECK-SILGEN --check-prefix=CHECK
// RUN: %target-swift-emit-sil -enable-experimental-cross-file-derivative-registration -I %S/Inputs -I %t %s | %FileCheck %s --check-prefix=CHECK-SIL --check-prefix=CHECK
// RUN: %target-build-swift -Xfrontend -enable-experimental-cross-file-derivative-registration -I %S/Inputs -I %t %s %t/CForeign.o

import CForeign

// TF-1087: Test derivative registration for foreign declaration (Clang-imported).
// Original SILDeclRef must have `isForeign` bit set correctly.

// CHECK-SILGEN-LABEL: // differentiability witness for cFunction
// CHECK-SILGEN: sil_differentiability_witness [serialized] [parameters 0] [results 0] @cFunction : $@convention(c) (Float) -> Float {
// CHECK-SILGEN:   vjp: @AD__cFunction__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SILGEN: }

// CHECK-SIL-LABEL: // differentiability witness for cFunction
// CHECK-SIL: sil_differentiability_witness [serialized] [parameters 0] [results 0] @cFunction : $@convention(c) (Float) -> Float {
// CHECK-SIL:   jvp: @AD__cFunction__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SIL:   vjp: @AD__cFunction__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SIL: }

// Check that original SIL function is correct.

// CHECK-SILGEN-LABEL: sil [serializable] [clang cFunction] @cFunction : $@convention(c) (Float) -> Float

@inlinable
@derivative(of: cFunction)
func vjpCFunction(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (cFunction(x), { $0 })
}

@_silgen_name("test_derivative")
@differentiable
func testDerivative(_ x: Float) -> Float {
  cFunction(x)
}

// CHECK-SILGEN-LABEL: sil hidden [ossa] @test_derivative : $@convention(thin) (Float) -> Float {
// CHECK-SILGEN: {{%.*}} = function_ref @cFunction : $@convention(c) (Float) -> Float
