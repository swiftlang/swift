// RUN: %target-typecheck-verify-swift

// Simple enumeration type
enum E1 {
  case First
  case Second(Int)
  case Third(Int, Double)
  case `default`
}

var e1: E1 = .First
e1 = .Second(5)
e1 = .Third(5, 3.14159)

e1 = .default  // SE-0071

// Generic enumeration type
enum E2<T> {
  case First
  case Second(T)
}

var e2a: E2<Int> = .First
e2a = .Second(5)
var e2b: E2 = .Second(5)
e2b = .First
var e2c: E2 = .First // expected-error{{generic parameter 'T' could not be inferred}}
