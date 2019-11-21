// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s

// expected-error @+1 {{@differentiable attribute only applies to function types}}
let _: @differentiable Float

let _: @differentiable (Float) -> Float
