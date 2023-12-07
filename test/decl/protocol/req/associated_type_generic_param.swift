// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: not %target-typecheck-verify-swift -disable-experimental-associated-type-inference

protocol P {
  associatedtype A = Int
}

struct S<A>: P {}

let x: String.Type = S<String>.A.self
