// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -verify %s

// REQUIRES: swift_feature_Lifetimes

struct Butt<T: ~Escapable>: ~Escapable {
	var x: T

	var xx: T {
		@_lifetime(copy self)
		borrow {
			return x
		}
	}
}

extension Butt: Escapable where T: Escapable {}

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
