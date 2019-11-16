// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s

// expected-error @+1 {{@differentiable attribute only applies to function types}}
let _: @differentiable Float

let _: @differentiable (Float) -> Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: @noDerivative Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (Float) -> @noDerivative Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: @differentiable (Float) -> @noDerivative Float

// expected-error @+1 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
let _: (@noDerivative Float) -> Float

// expected-error @+2 {{'@noDerivative' may only be used on parameters of '@differentiable' function types}}
// expected-error @+1 {{'@noDerivative' must not be used on variadic parameters}}
let _: (Float, @noDerivative Float...) -> Float

let _: @differentiable (@noDerivative Float, Float) -> Float

// expected-error @+1 {{'@noDerivative' must not be used on variadic parameters}}
let _: @differentiable (Float, @noDerivative Float...) -> Float
