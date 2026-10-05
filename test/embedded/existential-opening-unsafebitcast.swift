// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -swift-version 5
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -swift-version 6 -verify-additional-prefix swift6-

// The 'sil-' expectations below show what the type checker is getting ahead
// of: an opened argument that reaches SIL is a hard error.
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -emit-sil -o /dev/null -enable-experimental-feature Embedded -swift-version 6 -verify -verify-additional-prefix swift6- -verify-additional-prefix sil-

// REQUIRES: swift_feature_Embedded

public protocol ClassBound: AnyObject { }

public func acceptAny<T>(_ value: T) { }

// ---------------------------------------------------------------------------
// An unconstrained generic parameter bound to an existential type.
// ---------------------------------------------------------------------------

// This one survives to a working binary today only because
// `unsafeBitCast(_:to:)` is `@_transparent`: mandatory inlining removes the
// generic apply before specialization runs, so no SIL error follows.
public func pointerFromObject(_ object: AnyObject) -> UnsafeRawPointer {
  // expected-swift6-warning@+2{{cannot open existential type 'AnyObject' when passing it as an argument to global function 'unsafeBitCast(_:to:)' in Embedded Swift}}
  // expected-swift6-note@+1{{coerce the argument to 'AnyObject' to pass the existential value without opening it}} {{30-30= as AnyObject}}
  return unsafeBitCast(object, to: UnsafeRawPointer.self)
}

// An ordinary generic callee cannot be specialized for the opened archetype,
// so the same opening is a hard error once it reaches SIL.
public func passClassBound(_ value: any ClassBound) {
  // expected-swift6-warning@+3{{cannot open existential type 'any ClassBound' when passing it as an argument to global function 'acceptAny' in Embedded Swift}}
  // expected-swift6-note@+2{{coerce the argument to 'any ClassBound' to pass the existential value without opening it}} {{18-18= as any ClassBound}}
  // expected-sil-error@+1{{cannot specialize generic function or default protocol method in this context}}
  acceptAny(value)
}

// The other direction has no existential argument to open, so it is accepted
// in both language modes.
public func objectFromPointer(_ pointer: UnsafeRawPointer) -> AnyObject {
  return unsafeBitCast(pointer, to: AnyObject.self)
}

public func pointerFromObjectCoerced(_ object: AnyObject) -> UnsafeRawPointer {
  return unsafeBitCast(object as AnyObject, to: UnsafeRawPointer.self)
}

public func passClassBoundCoerced(_ value: any ClassBound) {
  acceptAny(value as any ClassBound)
}

// 'Unmanaged' constrains its generic parameter to 'AnyObject', which the
// existential satisfies, so nothing is opened here either.
public func pointerFromObjectUnmanaged(
  _ object: AnyObject
) -> UnsafeMutableRawPointer {
  return Unmanaged.passUnretained(object).toOpaque()
}
