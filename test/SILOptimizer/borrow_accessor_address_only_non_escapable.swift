// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowAndMutateAccessors -verify %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowAndMutateAccessors

struct Butt<T: ~Escapable>: ~Escapable {
	var x: T

	var xx: T {
		@_lifetime(copy self)
		borrow {
			return x
		}
	}
}

struct Tubb<T>: ~Escapable {
	var x: T

	@_lifetime(immortal)
	init() { fatalError() }

	var xx: T {
		borrow {
			return x
		}
	}
}
