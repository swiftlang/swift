// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s         -swift-version 5 -emit-module -DM -module-name M -emit-module-path %t/M.swiftmodule
// RUN: %target-swift-frontend %s -verify -swift-version 5 -typecheck -I %t -enable-upcoming-feature ExistentialAny

// Test that a protocol that requires 'any' *only* when the feature is enabled
// is diagnosed as expected when said protocol originates in a different module.
// In other words, test that deserialization does not affect 'any' migration
// diagnostics.

#if M
public protocol P {}
#else
import M
func test(_: P) {} // expected-error {{use of protocol 'P' as a type must be written 'any P'}}
#endif
