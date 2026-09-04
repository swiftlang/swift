// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

struct J<A> { }

// expected-error@+1 {{same-type constraint 'Self' == 'J<Self.A>' is recursive}}
extension P where Self == J<A> {
  static func just(_: A) { }
}
