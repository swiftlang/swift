// RUN: %target-typecheck-verify-swift
extension SIMD2: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD3: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD4: AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD8: AdditiveArithmetic where Scalar: FloatingPoint { }
