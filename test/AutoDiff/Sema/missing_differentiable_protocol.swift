// RUN: %target-swift-frontend -typecheck -verify %s

// Tests that Sema fails gracefully when the `_Differentiation` module is not imported.

// expected-error @+1 {{'@differentiable' used without importing module '_Differentiation'}}
let _: @differentiable(reverse) (Float) -> Float

// expected-error @+1 2 {{'@differentiable' used without importing module '_Differentiation'}}
func hasDifferentiableFunctionArg<T>(_ f: @differentiable(reverse) (T) -> T) {}
