//===--- SlabSpan.swift ---------------------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence -enable-experimental-feature Span -enable-experimental-feature AddressableTypes -enable-experimental-feature ValueGenerics)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ValueGenerics
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Span
// REQUIRES: swift_feature_LifetimeDependence

import StdlibUnittest

var suite = TestSuite("SlabStorageProperty")
defer { runAllTests() }

suite.test("Slab.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var s = Slab<5, Int>(repeating: 0)
  s[3] = .random(in: 0..<1000)
  let storage = s.storage
  expectEqual(storage.count, s.count)
  for i in 0..<s.count {
    expectEqual(storage[i], s[i])
  }
}
