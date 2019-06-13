// RUN: %target-swift-frontend -emit-sil -verify %s

// Test supported `br` and `cond_br` terminators.

@differentiable
func branch(_ x: Float) -> Float {
  if x > 0 {
    return x
  } else if x < 10 {
    return x
  }
  return x
}

// Test currently unsupported `switch_enum` terminator.

enum Enum {
  case a(Float)
  case b(Float)
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func switch_enum(_ e: Enum, _ x: Float) -> Float {
  // expected-note @+1 {{differentiating control flow is not yet supported}}
  switch e {
    case let .a(a): return a
    case let .b(b): return b
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
