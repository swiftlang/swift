//===--- With.swift -------------------------------------------------------===//
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

/// Private compiler-internal implementation of the `with` function
/// that is implicitly available on all values.
@_disfavoredOverload
public func _with<T>(_ value: T) -> ((inout T) -> Void) -> T {
  { modify in
    var copy = value
    modify(&copy)
    return copy
  }
}
