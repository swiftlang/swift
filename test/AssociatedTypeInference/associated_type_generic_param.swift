// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A = Int
}

struct S<A>: P {}

let x: String.Type = S<String>.A.self
