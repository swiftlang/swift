// RUN: %target-typecheck-verify-swift

public struct NotFrozen: ~Copyable {
	deinit {}

	public consuming func notInlinable() {
		discard self
	}

	@inlinable
	public consuming func inlinable() {
		// expected-warning @+1 {{'discard' statement cannot be used in an '@inlinable' function inside of type 'NotFrozen', which is not '@frozen'}}
		discard self
	}

	@_alwaysEmitIntoClient
	public consuming func aeic() {
		// expected-error @+1 {{'discard' statement cannot be used in an '@_alwaysEmitIntoClient' function inside of type 'NotFrozen', which is not '@frozen'}}
		discard self
	}

	@_transparent
	public consuming func transparent() {
		// expected-error @+1 {{'discard' statement cannot be used in a '@_transparent' function inside of type 'NotFrozen', which is not '@frozen'}}
		discard self
	}
}

@frozen
public struct Frozen: ~Copyable {
	deinit {}

	public consuming func notInlinable() {
		discard self
	}

	@inlinable
	public consuming func inlinable() {
		discard self
	}
}

@usableFromInline
internal struct NotFrozenUFI: ~Copyable {
	deinit {}

	public consuming func notInlinable() {
		discard self
	}

	@inlinable
	public consuming func inlinable() {
		// expected-warning @+1 {{'discard' statement cannot be used in an '@inlinable' function inside of type 'NotFrozenUFI', which is not '@frozen'}}
		discard self
	}

	@_alwaysEmitIntoClient
	public consuming func aeic() {
		// expected-error @+1 {{'discard' statement cannot be used in an '@_alwaysEmitIntoClient' function inside of type 'NotFrozenUFI', which is not '@frozen'}}
		discard self
	}

	@_transparent
	public consuming func transparent() {
		// expected-error @+1 {{'discard' statement cannot be used in a '@_transparent' function inside of type 'NotFrozenUFI', which is not '@frozen'}}
		discard self
	}
}

