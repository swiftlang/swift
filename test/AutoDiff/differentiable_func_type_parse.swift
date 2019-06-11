// RUN: %target-swift-frontend -parse -verify %s

let a: @differentiable (Float) -> Float // okay

let b: @differentiable(linear) (Float) -> Float // okay

// Generic type test.
struct A<T: Differentiable> {
  func foo() {
    let _: @differentiable(linear) (T) -> T // okay
  }
}
