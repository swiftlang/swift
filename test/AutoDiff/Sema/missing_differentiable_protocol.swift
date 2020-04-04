// RUN: %target-swift-frontend -typecheck -verify %s

// Tests that Sema fails gracefully when the `_Differentiation` module is not imported.

// expected-error @+1 {{'@differentiable' attribute used without importing module '_Differentiation'}}
let _: @differentiable (Float) -> Float

// expected-error @+1 2 {{'@differentiable' attribute used without importing module '_Differentiation'}}
func hasDifferentiableFunctionArg<T>(_ f: @differentiable (T) -> T) {}
