// RUN: %target-swift-frontend -disable-availability-checking -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowAndMutateAccessors -typecheck -verify %s 

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowAndMutateAccessors

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

struct Loan<Lessor: ~Copyable>: ~Escapable {
	let _lessor: Builtin.Borrow<Lessor>

	@_lifetime(borrow lessor)
	init(_ lessor: borrowing Lessor) {
		self._lessor = Builtin.makeBorrow(lessor)
	}

	var lessor: Lessor {
		borrow { return Builtin.dereferenceBorrow(self._lessor) }
	}
}
