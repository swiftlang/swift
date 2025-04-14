// RUN: %target-swift-frontend -emit-sil -verify %s

import _Differentiation

// Test supported `br`, `cond_br`, and `switch_enum` terminators.

@differentiable(reverse)
func branch(_ x: Float) -> Float {
  if x > 0 {
    return x
  } else if x < 10 {
    return x
  }
  return x
}

enum Enum {
  case a(Float)
  case b(Float)
}

@differentiable(reverse)
func enum_nonactive1(_ e: Enum, _ x: Float) -> Float {
  switch e {
    case .a: return x
    case .b: return x
  }
}

@differentiable(reverse)
func enum_nonactive2(_ e: Enum, _ x: Float) -> Float {
  switch e {
    case let .a(a): return x + a
    case let .b(b): return x + b
  }
}

// Test loops.

@differentiable(reverse)
func for_loop(_ x: Float) -> Float {
  var result: Float = x
  for _ in 0..<3 {
    result = result * x
  }
  return result
}

@differentiable(reverse)
func while_loop(_ x: Float) -> Float {
  var result = x
  var i = 1
  while i < 3 {
    result = result * x
    i += 1
  }
  return result
}

@differentiable(reverse)
func nested_loop(_ x: Float) -> Float {
  var outer = x
  for _ in 1..<3 {
    outer = outer * x

    var inner = outer
    var i = 1
    while i < 3 {
      inner = inner / x
      i += 1
    }
    outer = inner
  }
  return outer
}

// TF-433: Test throwing functions.

func rethrowing(_ x: () throws -> Void) rethrows -> Void {}

@differentiable(reverse)
func testTryApply(_ x: Float) -> Float {
  rethrowing({})
  return x
}

// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func withoutDerivative<T : Differentiable, R: Differentiable>(
  at x: T, in body: (T) throws -> R
) rethrows -> R {
  // expected-note @+1 {{expression is not differentiable}}
  try body(x)
}

// Tests active `try_apply`.
// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func testNilCoalescing(_ maybeX: Float?) -> Float {
  // expected-note @+1 {{expression is not differentiable}}
  return maybeX ?? 10
}

// Test unsupported differentiation of active enum values.

// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func enum_active(_ x: Float) -> Float {
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  let e: Enum
  if x > 0 {
    e = .a(x)
  } else {
    e = .b(x)
  }
  switch e {
    case let .a(a): return x + a
    case let .b(b): return x + b
  }
}

enum Tree : Differentiable & AdditiveArithmetic {
  case leaf(Float)
  case branch(Float, Float)

  typealias TangentVector = Self
  static var zero: Self { .leaf(0) }

  // expected-error @+1 {{function is not differentiable}}
  @differentiable(reverse)
  // TODO(TF-956): Improve location of active enum non-differentiability errors
  // so that they are closer to the source of the non-differentiability.
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  static func +(_ lhs: Self, _ rhs: Self) -> Self {
    switch (lhs, rhs) {
    case let (.leaf(x), .leaf(y)):
      return .leaf(x + y)
    case let (.branch(x1, x2), .branch(y1, y2)):
      return .branch(x1 + x2, y1 + y2)
    default:
      fatalError()
    }
  }

  // expected-error @+1 {{function is not differentiable}}
  @differentiable(reverse)
  // TODO(TF-956): Improve location of active enum non-differentiability errors
  // so that they are closer to the source of the non-differentiability.
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  static func -(_ lhs: Self, _ rhs: Self) -> Self {
    switch (lhs, rhs) {
    case let (.leaf(x), .leaf(y)):
      return .leaf(x - y)
    case let (.branch(x1, x2), .branch(y1, y2)):
      return .branch(x1 - x2, y1 - y2)
    default:
      fatalError()
    }
  }
}

// TODO(TF-957): Improve non-differentiability errors for for-in loops
// (`Collection.makeIterator` and `IteratorProtocol.next`).
// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
func loop_array(_ array: [Float]) -> Float {
  // expected-note@-1 {{when differentiating this function definition}}
  var result: Float = 1
  for x in array {
    // expected-note@-1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'}}
    result = result * x
  }
  return result
}
