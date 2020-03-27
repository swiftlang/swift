// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s

// SWIFT_ENABLE_TENSORFLOW
// Expected to fail on `tensorflow` branch because the `Differentiable` protocol
// currently exists in the core stdlib, not in the `_Differentiation` module.
// XFAIL: tensorflow
// SWIFT_ENABLE_TENSORFLOW END

// Tests that Sema fails gracefully when the `Differentiable` protocol is missing.

// expected-error @+2 {{parameter type 'Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
// expected-error @+1 {{result type 'Float' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
let _: @differentiable (Float) -> Float

// expected-error @+2 {{parameter type 'T' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
// expected-error @+1 {{result type 'T' does not conform to 'Differentiable', but the enclosing function type is '@differentiable'}}
func hasDifferentiableFunctionArg<T>(_ f: @differentiable (T) -> T) {}
