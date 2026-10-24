// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t
// RUN: %target-typecheck-verify-swift -I %t -enable-library-evolution -verify-additional-prefix resilient-

// With a deployment target where MyKit provides the type, there's no copy of
// the type for conformances to apply to instead.
// RUN: %target-swift-frontend -typecheck -I %t -enable-library-evolution %s -target %target-cpu-apple-macosx99 2>&1 | %FileCheck %s --allow-empty --check-prefix=NEW
// NEW-NOT: error

// REQUIRES: OS=macosx

import MyKit

// Before MyKit provides 'Point', this module uses its own copy of it, and
// MyKit's metadata for it doesn't see conformances declared for the copy.

extension Point: @retroactive CustomStringConvertible { // expected-error {{conformance of back-deployed type 'Point' to resilient protocol 'CustomStringConvertible' must not be available before macOS 99}}
  // expected-note@-1 {{this module uses its own copy of 'Point' before macOS 99, and the conformance would not apply to 'Point' from 'MyKit'}}
  // expected-note@-2 {{add '@available' attribute to enclosing extension}}
  public var description: String { "" }
}

// A conformance that's only available once MyKit provides the type is fine.
@available(macOS 99, *)
extension Point: @retroactive CustomDebugStringConvertible {
  public var debugDescription: String { "" }
}

// Marker protocols, like Sendable, can't be queried at runtime, so they have
// no conformance records.
extension Holder: @retroactive @unchecked Sendable {}

@_marker public protocol Marker {}
extension Point: Marker {}

// This module's own protocols are only resilient with library evolution.
public protocol LocalProto {
  func f()
}

extension Point: LocalProto { // expected-resilient-error {{conformance of back-deployed type 'Point' to resilient protocol 'LocalProto' must not be available before macOS 99}}
  // expected-resilient-note@-1 {{this module uses its own copy of 'Point' before macOS 99, and the conformance would not apply to 'Point' from 'MyKit'}}
  // expected-resilient-note@-2 {{add '@available' attribute to enclosing extension}}
  public func f() {}
}
