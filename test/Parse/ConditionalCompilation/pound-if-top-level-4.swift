// RUN: %target-typecheck-verify-swift

// https://bugs.swift.org/browse/SR-4426
// '#if' in top-level code that contains only decls should not disturb forward reference.

typealias A = B

#if false
func foo() {}
#endif

struct B {}

// If '#if' contains active non-decls, we don't support forward reference.
typealias C = D // expected-error {{use of undeclared type 'D'}}

#if true
print("ok")
#endif

struct D {}
