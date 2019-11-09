// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: @differentiable (Float) -> Float
