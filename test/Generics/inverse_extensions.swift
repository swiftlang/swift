// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_SuppressedAssociatedTypes

struct Turtle<T> {}
extension Turtle where T: ~Copyable {} // expected-error {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

struct Rabbit<T> where T: ~Copyable {}
extension Rabbit where T: ~Escapable {} // expected-error {{'T' required to be 'Escapable' but is marked with '~Escapable'}}

protocol P {}
extension P where Self: ~Escapable {} // expected-error {{'Self' required to be 'Escapable' but is marked with '~Escapable'}}

protocol HasAssoc {
  associatedtype A
}
extension HasAssoc where Self.A: ~Copyable {}
// expected-error@-1 {{'Self.A' required to be 'Copyable' but is marked with '~Copyable'}}

class Box<T: ~Copyable> {}
extension Box where T: ~Copyable {}
