// RUN: %target-typecheck-verify-swift -swift-version 5

 func consume<T>(_ x: T) {} // Suppress unused variable warnings

 func shuffle_through_initialization() {
   let a = (x: 1, y: 2)
   let b: (y: Int, x: Int)
   b = a // expected-warning {{expression shuffles the elements of this tuple}}
   consume(b)
 }

  func shuffle_through_destructuring() {
   let a = (x: 1, y: 2)
   let (y: b, x: c) = a // expected-warning {{expression shuffles the elements of this tuple}}
   consume((b, c))
 }

  func shuffle_through_call() {
   func foo(_ : (x: Int, y: Int)) {}
   foo((y: 5, x: 10)) // expected-warning {{expression shuffles the elements of this tuple}}
 }

  func shuffle_through_cast() {
   let x = ((a: Int(), b: Int()) as (b: Int, a: Int)).0 // expected-warning {{expression shuffles the elements of this tuple}}

   // Ah, the famous double-shuffle
   let (c1, (c2, c3)): (c: Int, (b: Int, a: Int)) = ((a: Int(), b: Int()), c: Int())
   // expected-warning@-1 {{expression shuffles the elements of this tuple}}
   // expected-warning@-2 {{expression shuffles the elements of this tuple}}
   consume((x, c1, c2, c3))
 }
