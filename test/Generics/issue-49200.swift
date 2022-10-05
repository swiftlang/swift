// RUN: %target-typecheck-verify-swift

protocol B {
  typealias A = Any
}

protocol D : B {
  // expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Any' and 'Self.A == AnyObject'}}
  // expected-error@-2 {{no type for 'Self.A' can satisfy both 'Self.A : AnyObject' and 'Self.A == Any'}}
  typealias A = AnyObject
}
