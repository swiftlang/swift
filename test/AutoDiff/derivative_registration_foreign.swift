// RUN: %target-swift-emit-sil -Xllvm -enable-experimental-cross-file-derivative-registration %s | %FileCheck %s

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
import Glibc
#elseif os(Windows)
import MSVCRT
#endif

// TF-1087: Test derivative registration for foreign declaration (Clang-imported).
// Original SILDeclRef must have `isForeign` bit set correctly.

// CHECK-LABEL: // differentiability witness for tan
// CHECK-LABEL: sil_differentiability_witness public_external [serialized] [parameters 0] [results 0] @tan : $@convention(c) (Double) -> Double {
// CHECK-LABEL:   jvp: @AD__tan__jvp_src_0_wrt_0 : $@convention(c) (Double) -> (Double, @owned @callee_guaranteed (Double) -> Double)
// CHECK-LABEL:   vjp: @AD__tan__vjp_src_0_wrt_0 : $@convention(c) (Double) -> (Double, @owned @callee_guaranteed (Double) -> Double)
// CHECK-LABEL: }

// Check that original SIL function is correct.

// CHECK-LABEL: sil [serializable] [readnone] [clang tan] @tan : $@convention(c) (Double) -> Double
// CHECK-NOT: sil shared [serializable] [readnone] @$sSo3tanyS2dFTO : $@convention(thin) (Double) -> Double

@derivative(of: tan)
@inlinable
func vjpTan(_ x: Double) -> (value: Double, pullback: (Double) -> Double) {
  let value = tan(x)
  return (value, { v in v * (1 + value * value) })
}

@differentiable
func testForeignDeclarationDerivative(_ x: Double) -> Double {
  tan(x)
}
