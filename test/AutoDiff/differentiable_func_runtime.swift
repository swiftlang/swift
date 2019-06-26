// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var DifferentiableFunctionTests = TestSuite("DifferentiableFunctions")

/*
// FIXME(TF-123): `@differentiable` function thunking with opaque
// abstraction patterns.
DifferentiableFunctionTests.test("ThunkingWithOpaqueAbstractionPattern") {
  func blackHole(_ x: Any) {}
  let f: @differentiable (Float) -> Float = { $0 }
  blackHole(f) 

  struct TF_123 {
    var f: @differentiable (Float) -> Float
  }
  _ = \TF_123.f
}
*/

runAllTests()
