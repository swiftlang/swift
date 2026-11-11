// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -verify-additional-prefix none-
// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -sanitize=address -verify-additional-prefix address-
// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -sanitize=thread -verify-additional-prefix thread-

// REQUIRES: swift_feature_Embedded

// Embedded restriction warnings are only emitted for code that would be active
// when compiling Embedded Swift. That is decided by a separate evaluation of
// '#if' conditions (EmbeddedBuildConfiguration in ASTGen), which must also
// understand 'moduleInstrumentation'. Using a generic method on an existential
// is just a convenient restriction to probe with: the warning appears only
// if the enclosing '#if moduleInstrumentation(...)' region is considered active.

protocol Q: AnyObject {
  func f<T>(_ t: T)
}

#if moduleInstrumentation(address)
func addressOnly(q: any Q) {
  q.f(1) // expected-address-warning {{cannot use generic instance method 'f' on a value of type 'any Q' in Embedded Swift}}
}
#endif

#if moduleInstrumentation(thread)
func threadOnly(q: any Q) {
  q.f(1) // expected-thread-warning {{cannot use generic instance method 'f' on a value of type 'any Q' in Embedded Swift}}
}
#endif
