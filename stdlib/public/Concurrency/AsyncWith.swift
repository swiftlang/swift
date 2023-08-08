//===--- AsyncWith.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// Non-async implementations defined in public/Core/With.swift

/// Async implemtation of the `with` function that is implicitly available on all values.
@inlinable
@_disfavoredOverload
@available(SwiftStdlib 5.1, *)
@backDeployed(before: SwiftStdlib 5.10)
public func _with<T>(_ value: T) -> ((inout T) async -> Void) async -> T {
  { modify in
    var copy = value
    await modify(&copy)
    return copy
  }
}

/// Async and throwing implementation of the `with` function that is implicitly available on all values.
@inlinable
@_disfavoredOverload
@available(SwiftStdlib 5.1, *)
@backDeployed(before: SwiftStdlib 5.10)
public func _with_throws<T>(_ value: T) -> ((inout T) async throws -> Void) async throws -> T {
  { modify in
    var copy = value
    try await modify(&copy)
    return copy
  }
}
