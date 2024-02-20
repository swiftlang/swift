// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference

protocol P {
  associatedtype A = Int
}

struct S<A>: P {}

// This is unfortunate but it is the behavior of Swift 5.10.
let x: Int.Type = S<String>.A.self
