// RUN: %target-swift-frontend -parse -verify %s

let a: @differentiable (Float) -> Float // okay

let b: @differentiable(linear) (Float) -> Float // okay
