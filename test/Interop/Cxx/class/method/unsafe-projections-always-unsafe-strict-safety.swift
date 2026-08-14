// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe -strict-memory-safety

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// The migration stub is only '@unsafe', not '@unsafe(always)'. That is
// invisible in the default mode, where an unacknowledged use of it is accepted
// outright, so check here that strict memory safety still flags it.

import UnsafeProjections

func useMigrationStub(_ sc: SelfContained) {
  _ = unsafe sc.__viewUnsafe()
  // expected-warning@-1 {{'__viewUnsafe()' is deprecated: renamed to 'view()'}}
  // expected-note@-2 {{use 'view()' instead}}

  _ = sc.__viewUnsafe()
  // expected-warning@-1 {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-warning@-2 {{'__viewUnsafe()' is deprecated: renamed to 'view()'}}
  // expected-note@-3 {{use 'view()' instead}}
  // expected-note@-4 {{reference to unsafe instance method '__viewUnsafe()'}}
  // expected-note@-5 {{reference to parameter 'sc' involves unsafe type 'SelfContained'}}
}
