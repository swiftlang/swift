// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t
// RUN: %target-typecheck-verify-swift -I %t -enable-library-evolution
// RUN: %target-typecheck-verify-swift -I %t -enable-library-evolution -target %target-cpu-apple-macosx99

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx

import MyKit

// Before MyKit provides 'Point', this module uses its own copy of it. The
// conformance records that this module emits for the copy also refer to
// MyKit's type, so that the runtime finds them when MyKit provides the type.

extension Point: @retroactive CustomStringConvertible {
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

extension Point: LocalProto {
  public func f() {}
}
