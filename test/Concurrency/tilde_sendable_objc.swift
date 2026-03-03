// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 6 \
// RUN:   -enable-experimental-feature TildeSendable \
// RUN:   -module-name main -I %t -verify -verify-ignore-unrelated

// REQUIRES: objc_interop
// REQUIRES: swift_feature_TildeSendable

//--- Test.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))
#define SWIFT_NONSENDABLE_ASSUMED __attribute__((__swift_attr__("@_nonSendable(_assumed)")))
#define SWIFT_SUPPRESS_SENDABLE __attribute__((__swift_attr__("~Sendable")))

@import Foundation;

// Test that `~Sendable` superseeds `@_nonSendable(_assumed)` on classes.

SWIFT_NONSENDABLE_ASSUMED
SWIFT_SUPPRESS_SENDABLE
@interface Parent : NSObject
@end

// Test that `Sendable` superseeds `@_nonSendable(_assumed)` and `~Sendable` from the parent.

SWIFT_NONSENDABLE_ASSUMED
SWIFT_SENDABLE
@interface SendableValue : Parent
@end

SWIFT_NONSENDABLE_ASSUMED
@interface NonSendableValue : Parent
@end

//--- main.swift
func testSendable<T: Sendable>(_: T) {}

public func test(p: Parent, v: SendableValue, ns: NonSendableValue) {
  testSendable(p) // expected-error {{type 'Parent' does not conform to the 'Sendable' protocol}}
  testSendable(v) // Ok (no diagnostics unable unavailable conformance associated with `Parent`).
  testSendable(ns) // expected-error {{conformance of 'NonSendableValue' to 'Sendable' is unavailable}}
}

