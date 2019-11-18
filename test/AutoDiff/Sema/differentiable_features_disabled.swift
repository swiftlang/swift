// RUN: %target-swift-frontend -typecheck -verify %s
// SWIFT_ENABLE_TENSORFLOW
// XFAIL: *

// expected-error @+1 {{differentiable programming is an experimental feature that is currently disabled}}
let _: @differentiable (Float) -> Float
