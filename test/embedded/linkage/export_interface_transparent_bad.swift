// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded

@_transparent
@export(interface) // expected-error {{'@export(interface)' cannot be applied to a global function 'bad()' in Embedded Swift}}
public func bad() { }

// `@export(implementation)` is fine with `@_transparent`.
@_transparent
@export(implementation)
public func okImplementation() { }

// `@_transparent` alone is unaffected.
@_transparent
public func okTransparent() { }
