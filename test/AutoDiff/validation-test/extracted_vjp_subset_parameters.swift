// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Differentiation

var ExtractedVJPSubsetTests = TestSuite("ExtractedVJPSubset")

@differentiable(reverse)
func multiply(_ a: Float, _ b: Float) -> Float {
  a * b
}

@differentiable(reverse, wrt: x)
func callSecond(_ x: Float,
                _ body: @differentiable(reverse) (Float, Float) -> Float)
    -> Float {
  body(2, x)
}

@differentiable(reverse)
func throwingMultiply(_ a: Float, _ b: Float) throws -> Float {
  a * b
}

@differentiable(reverse, wrt: x)
func callThrowingSecond(
  _ x: Float,
  _ body: @differentiable(reverse) (Float, Float) throws -> Float
) throws -> Float {
  try body(2, x)
}

// The extracted VJP differentiates both inputs, but only the second input
// is active at each call.  Its gradient is 2, not the first input's 3.
ExtractedVJPSubsetTests.test("nonthrowing, inactive first parameter") {
  expectEqual(Float(2), gradient(at: Float(3)) { x in
    callSecond(x, multiply)
  })
}

ExtractedVJPSubsetTests.test("throwing, inactive first parameter") {
  expectEqual(Float(2), gradient(at: Float(3)) { x in
    try! callThrowingSecond(x, throwingMultiply)
  })
}

runAllTests()
