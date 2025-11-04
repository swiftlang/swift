// RUN: %target-swift-frontend -disable-experimental-parser-round-trip -disable-availability-checking -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes -typecheck -verify %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes

import Builtin

@_lifetime(copy x)
func a<T: ~Copyable>(x: Builtin.Borrow<T>) -> Builtin.Borrow<T> {
	return x
}

struct EscapableContainingBorrow<T> { // expected-note {{consider adding '~Escapable'}}
	var x: Builtin.Borrow<T> // expected-error {{has non-Escapable type}}
}
struct NonescapableContainingBorrow<T>: ~Escapable {
	var x: Builtin.Borrow<T>
}
