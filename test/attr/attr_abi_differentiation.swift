// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: differentiable_programming

// Differentiation-related parts of attr_abi.swift.

import _Differentiation

// @noDerivative should match
@abi(func noDerivativeTest1(_ a: @differentiable(reverse) (@noDerivative Double, Double) -> Double))
func noDerivativeTest1(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {}

@abi(func noDerivativeTest2(_ a: @differentiable(reverse) (Double, Double) -> Double)) // expected-error {{parameter 'a' type '@differentiable(reverse) (Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (@noDerivative Double, Double) -> Double'}}
func noDerivativeTest2(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {} // expected-note {{should match type here}}

@abi(func noDerivativeTest3(_ a: @differentiable(reverse) (@noDerivative Double, Double) -> Double)) // expected-error {{parameter 'a' type '@differentiable(reverse) (@noDerivative Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (Double, Double) -> Double'}}
func noDerivativeTest3(_: @differentiable(reverse) (Double, Double) -> Double) {} // expected-note {{should match type here}}

// @derivative, @differentiable, @transpose, @_noDerivative -- banned in @abi
// Too complex to infer or check
// TODO: Figure out if there's something we could do here.
@abi(@differentiable(reverse) func differentiable1(_ x: Float) -> Float) // expected-error {{unused 'differentiable' attribute in '@abi'}} {{6-30=}}
@differentiable(reverse) func differentiable1(_ x: Float) -> Float { x }

@abi(@differentiable(reverse) func differentiable2(_ x: Float) -> Float) // expected-error {{unused 'differentiable' attribute in '@abi'}} {{6-30=}}
func differentiable2(_ x: Float) -> Float { x }

@abi(func differentiable3(_ x: Float) -> Float)
@differentiable(reverse) func differentiable3(_ x: Float) -> Float { x }

@abi(
  @derivative(of: differentiable1(_:)) // expected-error {{unused 'derivative' attribute in '@abi'}} {{3-40=}}
  func derivative1(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
@derivative(of: differentiable1(_:))
func derivative1(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

@abi(
  @derivative(of: differentiable2(_:)) // expected-error {{unused 'derivative' attribute in '@abi'}} {{3-40=}}
  func derivative2(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
func derivative2(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

@abi(
  func derivative3(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
@derivative(of: differentiable3(_:))
func derivative3(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

struct Transpose<T: Differentiable & AdditiveArithmetic> where T == T.TangentVector {
  func fn1(_ x: T, _ y: T) -> T { x + y }
  func fn2(_ x: T, _ y: T) -> T { x + y }
  func fn3(_ x: T, _ y: T) -> T { x + y }

  @abi(
    @transpose(of: fn1, wrt: (0, 1)) // expected-error {{unused 'transpose' attribute in '@abi'}} {{5-38=}}
    func t_fn1(_ result: T) -> (T, T)
  )
  @transpose(of: fn1, wrt: (0, 1))
  func t_fn1(_ result: T) -> (T, T) { (result, result) }

  @abi(
    @transpose(of: fn2, wrt: (0, 1)) // expected-error {{unused 'transpose' attribute in '@abi'}} {{5-38=}}
    func t_fn2(_ result: T) -> (T, T)
  )
  func t_fn2(_ result: T) -> (T, T) { (result, result) }

  @abi(
    func t_fn3(_ result: T) -> (T, T)
  )
  @transpose(of: fn3, wrt: (0, 1))
  func t_fn3(_ result: T) -> (T, T) { (result, result) }
}

struct NoDerivative {
  @abi(@noDerivative func fn1()) // expected-error {{unused 'noDerivative' attribute in '@abi'}} {{8-21=}}
  @noDerivative func fn1() {}

  @abi(@noDerivative func fn2()) // expected-error {{unused 'noDerivative' attribute in '@abi'}} {{8-21=}}
  func fn2() {}

  @abi(func fn3())
  @noDerivative func fn3() {}
}
