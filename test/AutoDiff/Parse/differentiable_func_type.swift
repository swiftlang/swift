// RUN: %target-swift-frontend -dump-parse -verify %s | %FileCheck %s

let a: @differentiable (Float) -> Float // okay
// CHECK: (pattern_named 'a'
// CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

let b: @differentiable(linear) (Float) -> Float // okay
// CHECK: (pattern_named 'b'
// CHECK-NEXT: (type_attributed attrs=@differentiable(linear)

// Generic type test.
struct A<T> {
  func foo() {
    let local: @differentiable(linear) (T) -> T // okay
    // CHECK: (pattern_named 'local'
    // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)
  }
}

// expected-error @+1 {{expected ')' in '@differentiable' attribute}}
let c: @differentiable(linear (Float) -> Float

// expected-error @+1 {{expected ')' in '@differentiable' attribute}}
let c: @differentiable(notValidArg (Float) -> Float

// expected-error @+1 {{unexpected argument 'notValidArg' in '@differentiable' attribute}}
let d: @differentiable(notValidArg) (Float) -> Float

// Using 'linear' as a type
struct B {
  struct linear {}
  let propertyB1: @differentiable (linear) -> Float // okay
  // CHECK: (pattern_named 'propertyB1'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyB2: @differentiable(linear) (linear) -> linear // okay
  // CHECK: (pattern_named 'propertyB2'
  // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)

  let propertyB3: @differentiable (linear, linear) -> linear // okay
  // CHECK: (pattern_named 'propertyB3'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyB4: @differentiable (linear, Float) -> linear // okay
  // CHECK: (pattern_named 'propertyB4'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyB5: @differentiable (Float, linear) -> linear // okay
  // CHECK: (pattern_named 'propertyB5'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyB6: @differentiable(linear) (linear, linear, Float, linear)
    -> Float // okay
  // CHECK: (pattern_named 'propertyB6'
  // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)

  // expected-error @+1 {{expected ')' in '@differentiable' attribute}}
  let propertyB7: @differentiable(linear (linear) -> Float
}

// Using 'linear' as a typealias
struct C {
  typealias linear = (C) -> C
  let propertyC1: @differentiable (linear) -> Float // okay
  // CHECK: (pattern_named 'propertyC1'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyC2: @differentiable(linear) (linear) -> linear // okay
  // CHECK: (pattern_named 'propertyC2'
  // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)

  let propertyC3: @differentiable linear // okay
  // CHECK: (pattern_named 'propertyC3'
  // CHECK-NEXT: (type_attributed attrs=@differentiable{{[^(]}}

  let propertyC4: linear // okay
  // CHECK: (pattern_named 'propertyC4'

  let propertyC5: @differentiable(linear) linear // okay
  // CHECK: (pattern_named 'propertyC5'
  // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)

  let propertyC6: @differentiable(linear) @convention(c) linear // okay
  // CHECK: (pattern_named 'propertyC6'
  // CHECK-NEXT: (type_attributed attrs=@differentiable(linear)
}
