// Just enough definition here to test the behavior of referencing the names.
public struct Borrow<Target: ~Copyable>: ~Escapable {
	@_lifetime(immortal)
	init() {}
}
public struct Inout<Target: ~Copyable>: ~Escapable, ~Copyable {
	@_lifetime(immortal)
	init() {}
}

@_marker public protocol Copyable {}
@_marker public protocol Escapable {}

public struct Int {}

// Make sure the stdlib is able to refer to its own declarations irrespective
// of feature flag settings.
public func referenceBorrow(x: Borrow<Int>) {}
public func referenceInout(x: consuming Inout<Int>) {}
