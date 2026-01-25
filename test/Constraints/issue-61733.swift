// RUN: %target-typecheck-verify-swift
// REQUIRES: asserts
// Issue: https://github.com/swiftlang/swift/issues/61733
// Verifies SE-0375 fix for (any P)? → (some P) conversion via ForceOptional

protocol P {}
struct S: P {}

func takesOptionalP(_: (some P)?) {}

func passOptional(foo: (any P)?) {
    takesOptionalP(foo)
    // expected-error @-1 {{value of optional type '(any P)?' must be unwrapped to a value of type 'any P'}}
    // expected-note @-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
    // expected-note @-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
}
