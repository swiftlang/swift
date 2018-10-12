// RUN: %target-typecheck-verify-swift -swift-version 5

func consume<T>(_ x: T) {} // Suppress unused variable warnings

func shuffle_through_initialization() {
  let a = (x: 1, y: 2)
  let b: (y: Int, x: Int)
  b = a // expected-error {{cannot assign value of type '(x: Int, y: Int)' to type '(y: Int, x: Int)'}}
  consume(b)
}

func shuffle_through_destructuring() {
  let a = (x: 1, y: 2)
  let (y: b, x: c) = a // expected-error {{tuple type '(x: Int, y: Int)' is not convertible to tuple '(y: _, x: _)'}}
  consume((b, c))
}

func shuffle_through_call() {
  func foo(_ : (x: Int, y: Int)) {}
  foo((y: 5, x: 10)) // expected-error {{cannot convert value of type '(y: Int, x: Int)' to expected argument type '(x: Int, y: Int)'}}
}

func shuffle_through_cast() {
  let x = ((a: Int(), b: Int()) as (b: Int, a: Int)).0 // expected-error {{cannot convert value of type '(a: Int, b: Int)' to type '(b: Int, a: Int)' in coercion}}

  // Ah, the famous double-shuffle
  let (c1, (c2, c3)): (c: Int, (b: Int, a: Int)) = ((a: Int(), b: Int()), c: Int())
  // expected-error@-1 {{cannot convert value of type '((a: Int, b: Int), c: Int)' to specified type '(c: Int, (b: Int, a: Int))'}}
  consume((x, c1, c2, c3))
}
