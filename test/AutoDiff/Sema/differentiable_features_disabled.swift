// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: @differentiable (Float) -> Float

// expected-error @+2 {{differentiable programming is an experimental feature that is currently disabled}}
// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: @differentiable (Float, @nondiff Float) -> Float

// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: (Float, @nondiff Float) -> Float

// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: @nondiff Float
