// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

import UnsafeProjections
import AlwaysUnsafeProjections

func useProjections(_ sc: SelfContained) {
  // An unsafe projection under its original name must be acknowledged.
  _ = sc.view() // expected-error {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'view()'}}

  _ = unsafe sc.view()

  _ = sc.pointer() // expected-error {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'pointer()'}}

  _ = unsafe sc.pointer()

  // Safe methods need no acknowledgement.
  _ = sc.value()
  _ = sc.selfContained()
}

func useMigrationStub(_ sc: SelfContained) {
  // The renamed spelling still works, and points at the original name.
  _ = unsafe sc.__viewUnsafe()
  // expected-warning@-1 {{'__viewUnsafe()' is deprecated: renamed to 'view()'}}
  // expected-note@-2 {{use 'view()' instead}}

  // The stub's name already says 'Unsafe', so it is only '@unsafe', not
  // '@unsafe(always)': existing code that calls it without acknowledgement
  // keeps compiling (unless strict memory safety is enabled).
  _ = sc.__viewUnsafe()
  // expected-warning@-1 {{'__viewUnsafe()' is deprecated: renamed to 'view()'}}
  // expected-note@-2 {{use 'view()' instead}}
}

// Inherited members are cloned into the derived type; both the original name
// and its stub have to come along.
func useInherited(_ d: InheritedDerived) {
  _ = unsafe d.view()
  _ = unsafe d.pointer()
  _ = d.value()

  _ = unsafe d.__viewUnsafe()
  // expected-warning@-1 {{'__viewUnsafe()' is deprecated: renamed to 'view()'}}
  // expected-note@-2 {{use 'view()' instead}}

  _ = d.view() // expected-error {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'view()'}}
}

// 'value', 'insert' and 'append' are carved out only for the C++ standard
// library, whose overlay provides same-named safe wrappers. A user type gets the
// original name plus a stub like any other projection, rather than being left
// renamed with no un-renamed spelling to migrate to.
func useNotStd(_ n: inout NotStd) {
  _ = unsafe n.value()
  _ = unsafe n.insert(1)
  _ = unsafe n.append(1)

  _ = unsafe n.__valueUnsafe()
  // expected-warning@-1 {{'__valueUnsafe()' is deprecated: renamed to 'value()'}}
  // expected-note@-2 {{use 'value()' instead}}

  _ = unsafe n.__insertUnsafe(1)
  // expected-warning@-1 {{'__insertUnsafe' is deprecated: renamed to 'insert(_:)'}}
  // expected-note@-2 {{use 'insert(_:)' instead}}
}
