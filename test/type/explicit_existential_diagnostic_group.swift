// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ExistentialAny -Werror ExistentialAny -swift-version 5 -verify-additional-prefix swift5-
// RUN: %if swift_future %{ %target-typecheck-verify-swift -swift-version future -Werror ExistentialAny -verify-additional-prefix future- %}

// REQUIRES: swift_feature_ExistentialAny

do {
  protocol P {}

  let _: P
  // expected-swift5-error@-1{{use of protocol 'P' as a type must be written 'any P'; this will be an error in a future Swift language mode}}
  // expected-future-error@-2 {{use of protocol 'P' as a type must be written 'any P'}}
  let _: ~Copyable
  // expected-swift5-error@-1 {{constraint that suppresses conformance requires 'any'; this will be an error in a future Swift language mode}}
  // expected-future-error@-2 {{constraint that suppresses conformance requires 'any'}}
}
