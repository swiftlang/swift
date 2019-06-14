// RUN: %target-swift-frontend -emit-sil -verify %s

// Test supported `br`, `cond_br`, and `switch_enum` terminators.

@differentiable
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

@differentiable
func enum_nonactive1(_ e: Enum, _ x: Float) -> Float {
  switch e {
    case .a: return x
    case .b: return x
  }
}

@differentiable
func enum_nonactive2(_ e: Enum, _ x: Float) -> Float {
  switch e {
    case let .a(a): return x + a
    case let .b(b): return x + b
  }
}

// Test unsupported differentiation of active enum values.

// expected-error @+1 {{function is not differentiable}}
@differentiable
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
  typealias AllDifferentiableVariables = Self
  static var zero: Self { .leaf(0) }

  // expected-error @+1 {{function is not differentiable}}
  @differentiable
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
  @differentiable
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

// Test loops.

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func loop(_ x: Float) -> Float {
  var result: Float = 1
  // expected-note @+1 {{differentiating loops is not yet supported}}
  for _ in 0..<3 {
    result += x
  }
  return x
}
