// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s

// expected-error @+1 {{@differentiable attribute only applies to function types}}
let _: @differentiable Float

let _: @differentiable (Float) -> Float

// expected-error @+1 {{'@nondiff' may only be used on parameters of '@differentiable' function types}}
let _: @nondiff Float

// expected-error @+1 {{'@nondiff' may only be used on parameters of '@differentiable' function types}}
let _: (Float) -> @nondiff Float

// expected-error @+1 {{'@nondiff' may only be used on parameters of '@differentiable' function types}}
let _: @differentiable (Float) -> @nondiff Float

// expected-error @+1 {{'@nondiff' may only be used on parameters of '@differentiable' function types}}
let _: (@nondiff Float) -> Float

// expected-error @+2 {{'@nondiff' may only be used on parameters of '@differentiable' function types}}
// expected-error @+1 {{'@nondiff' must not be used on variadic parameters}}
let _: (Float, @nondiff Float...) -> Float

let _: @differentiable (@nondiff Float, Float) -> Float

// expected-error @+1 {{'@nondiff' must not be used on variadic parameters}}
let _: @differentiable (Float, @nondiff Float...) -> Float
