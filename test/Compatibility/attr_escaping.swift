// RUN: %target-typecheck-verify-swift
// REQUIRES: SWIFT_VERSION=3

// This is allowed, in order to keep source compat with Swift version 3.0.
func takesVarargsOfFunctionsExplicitEscaping(_ fns: @escaping () -> ()...) {} 

func takesVarargsOfFunctions(_ fn: () -> (), _ fns: () -> ()...) {
		// expected-note@-1{{parameter 'fn' is implicitly non-escaping}}
	takesVarargsOfFunctionsExplicitEscaping(fns[0], fns[1]) // ok
	takesVarargsOfFunctionsExplicitEscaping(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
} 
