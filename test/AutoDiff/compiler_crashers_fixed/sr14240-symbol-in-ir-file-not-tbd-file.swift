// RUN: %target-run-simple-swift(-Xfrontend -requirement-machine=off)

// rdar://82240971 – Temporarily disable this test on non-macOS platforms until
// the CI is fixed to not produce 'Invalid device: iPhone 8'.
// REQUIRES: OS=macosx

// REQUIRES: executable_test

// SR-14240: Error: symbol 'powTJfSSpSr' (powTJfSSpSr) is in generated IR file,
// but not in TBD file.

import _Differentiation

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(Windows)
  import CRT
#else
#error("Unsupported platform")
#endif

@inlinable
@derivative(of: pow)
func powVJP(
  _ base: Double, _ exponent: Double
) -> (value: Double, pullback: (Double) -> (Double, Double)) {
  fatalError()
}
