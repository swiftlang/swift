// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var CustomDerivativesTests = TestSuite("CustomDerivatives")

// Not differentiable!
func foo(_ x: Float) -> Float {
  var x = x
  x *= 2
  return x
}

CustomDerivativesTests.test("Make a differentiable function") {
  let diffableFoo = differentiableFunction { x in
    (value: foo(x), pullback: { v in v * x * 2 })
  }
  expectEqual(20, gradient(at: 10, in: diffableFoo))
}

CustomDerivativesTests.test("Differentiation of @autodiff function") {
  let diffableFoo = differentiableFunction { x in
    (value: foo(x), pullback: { v in v * x * 2 })
  }
  expectEqual(20, gradient(at: 10, in: { x in diffableFoo(x) }))
}

runAllTests()
