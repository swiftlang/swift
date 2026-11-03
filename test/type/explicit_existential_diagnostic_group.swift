// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ExistentialAny -Werror ExistentialAny -swift-version 5

// REQUIRES: swift_feature_ExistentialAny

do {
  protocol P {}

  let _: P
  // expected-error@-1{{use of protocol 'P' as a type must be written 'any P'}}
  let _: ~Copyable
  // expected-error@-1 {{constraint that suppresses conformance requires 'any'}}
}
