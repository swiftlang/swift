// RUN: %target-typecheck-verify-swift
extension SIMD2: @retroactive AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD3: @retroactive AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD4: @retroactive AdditiveArithmetic where Scalar: FloatingPoint { }
extension SIMD8: @retroactive AdditiveArithmetic where Scalar: FloatingPoint { }
