// RUN: %target-swift-frontend -enable-library-evolution -emit-sil -verify %s

public struct Consumable: ~Copyable {}

public func consume(_: consuming Consumable) {}

public struct NotFrozen: ~Copyable {
	public var field: Consumable

	public consuming func notInlinable() {
		consume(field)
	}

	@inlinable
	public consuming func inlinable() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}

	@_alwaysEmitIntoClient
	public consuming func aeic() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}

	@_transparent
	public consuming func transparent() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}
}

@frozen
public struct Frozen: ~Copyable {
	public var field: Consumable

	public consuming func notInlinable() {
		consume(field)
	}

	@inlinable
	public consuming func inlinable() {
		consume(field)
	}
}

@usableFromInline
internal struct NotFrozenUFI: ~Copyable {
	public var field: Consumable

	public consuming func notInlinable() {
		consume(field)
	}

	@inlinable
	public consuming func inlinable() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}

	@_alwaysEmitIntoClient
	public consuming func aeic() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}

	@_transparent
	public consuming func transparent() {
		// expected-error @+1 {{consumed but not reinitialized}}
		consume(field) // expected-note{{}}
	}
}

