//===--- Borrowing.swift --- test Span-borrowing behavior -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-swift-frontend -enable-experimental-feature Span -enable-experimental-feature LifetimeDependence -typecheck %s -verify

// REQUIRES: swift_feature_LifetimeDependence

import StdlibUnittest

/*
 What's missing in the command line?
 We expect an error message saying that there are overlapping accesses to `a`.

 error: overlapping accesses to 'a', but modification requires exclusive access
 */

@available(SwiftStdlib 6.1, *)
func testSpan() {
  let capacity = 4
  var a = ContiguousArray(0..<capacity)
  let bytes = a.storage.bytes
  expectEqual(bytes.byteCount, capacity*MemoryLayout<Int>.stride)

  a.append(capacity)
}
