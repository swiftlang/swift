// SWIFT_ENABLE_TENSORFLOW
// NOTE(TF-801): Test is disabled because `tensorflow` branch already defines
// `SIMD{X}: AdditiveArithmetic` conformances in the stdlib to support
// `SIMD{X}: Differentiable` conformances.
// Upstreaming `SIMD{X}: AdditiveArithmetic` is not yet possible because it is
// an ABI breaking change.
UNSUPPORTED: true

// RUN: %target-typecheck-verify-swift
extension SIMD2: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD3: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD4: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD8: AdditiveArithmetic where Scalar: FloatingPoint { }
