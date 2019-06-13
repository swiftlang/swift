// RUN: %target-swift-frontend -parse -verify %s

let a: @differentiable (Float) -> Float // okay

let b: @differentiable(linear) (Float) -> Float // okay

// Generic type test.
struct A<T: Differentiable> {
  func foo() {
    let _: @differentiable(linear) (T) -> T // okay
  }
}

// expected-error @+1 {{expected ')' after 'linear' in '@differentiable' attribute}}
let c: @differentiable(linear (Float) -> Float

// expected-error @+1 {{unexpected argument 'notValidArg' in '@differentiable' attribute}}
let c: @differentiable(notValidArg) (Float) -> Float

// Using 'linear' as a type
struct A {
  struct linear : Differentiable {}
  let property: @differentiable (linear) -> Float // okay
  let property: @differentiable(linear) (linear) -> linear // okay
  let property: @differentiable (linear, linear) -> linear // okay
  let property: @differentiable (linear, Float) -> linear // okay
  let property: @differentiable (Float, linear) -> linear // okay
  let property: @differentiable(linear) (linear, linear, Float, linear)
    -> Float // okay
  // expected-error @+1 {{expected ')' after 'linear' in '@differentiable' attribute}}
  let property: @differentiable(linear (linear) -> Float
}

// Using 'linear' as a typealias
struct B {
  typealias linear = (Float) -> Float
  let property: @differentiable (linear) -> Float // okay
  let property: @differentiable(linear) (linear) -> linear // okay
  let property: @differentiable linear // okay
  let property: linear // okay
  // FIXME(bartchr): TF-576 have this able to be parsed.
  //let property: @differentiable(linear) linear // okay
}
