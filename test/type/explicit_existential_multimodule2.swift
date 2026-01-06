// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s         -swift-version 5 -emit-module -DM -module-name M -emit-module-path %t/M.swiftmodule
// RUN: %target-swift-frontend %s -verify -swift-version 5 -typecheck -I %t -enable-upcoming-feature ExistentialAny

// REQUIRES: swift_feature_ExistentialAny

// Test that a protocol that requires 'any' *only* under ExistentialAny
// is diagnosed as expected with the feature enabled when said protocol
// originates in a different module.
// In other words, test that deserialization does not affect 'any' migration
// diagnostics.

#if M
public protocol P {}
#else
import M
func test(_: P) {}
// expected-warning@-1 {{use of protocol 'P' as a type must be written 'any P'}}
#endif
