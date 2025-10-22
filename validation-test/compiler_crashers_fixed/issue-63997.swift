// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

struct J<A> { }

// expected-error@+2 {{same-type constraint 'Self' == 'J<Self.A>' is recursive}}
// expected-error@+1 {{no type for 'Self' can satisfy both 'Self == J<Self.A>' and 'Self : P'}}
extension P where Self == J<A> {
  static func just(_: A) { }
}
