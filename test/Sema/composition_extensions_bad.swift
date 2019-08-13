// RUN: %target-typecheck-verify-swift

// Make sure that we don't crash when we get erroneous input.

class C {
  func c() {}
}

class D : C {
  func d() {}
}

class E {
  func e() {}
}

protocol P {
  func p()
}

protocol Q {
  func q()
}

extension E & C { }
  // expected-error@-1 {{protocol-constrained type cannot contain class 'C' because it already contains class 'E'}}

extension E & D & P { }
  // expected-error@-1 {{protocol-constrained type cannot contain class 'D' because it already contains class 'E'}}

extension C & P & Q & E { }
  // expected-error@-1 {{protocol-constrained type cannot contain class 'E' because it already contains class 'C'}}

extension C & (D, E) { }
  // expected-error@-1 {{non-protocol, non-class type '(D, E)' cannot be used within a protocol-constrained type}}

extension P & (D, E) { }
  // expected-error@-1 {{non-protocol, non-class type '(D, E)' cannot be used within a protocol-constrained type}}

extension P & ((Q & E) & D) { }
  // expected-error@-1 {{protocol-constrained type cannot contain class 'D' because it already contains class 'E'}}

extension AnyObject { }
  // expected-error@-1 {{non-nominal type 'AnyObject' cannot be extended}}
