//===--- InlineStorage.swift ----------------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence -enable-experimental-feature Span -enable-experimental-feature AddressableTypes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Span

import StdlibUnittest

var suite = TestSuite("InlineTypesStorageProperty")
defer { runAllTests() }

suite.test("CollectionOfOne.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var s = "A long string that is absolutely not smol at all."
  let u = Array(s.utf8)
  let c = CollectionOfOne(s)
  s = ""
  var storage = c.storage
  expectEqual(storage.count, 1)
  let v = Array(storage[0].utf8)
  expectEqual(u, v)
}

suite.test("SIMD2.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD2(repeating: 42)
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD3.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD3(repeating: 42)
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD4.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD4(repeating: 42)
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD8.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD8(repeating: UInt16(42))
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD16.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD16(repeating: UInt16(42))
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD32.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD32(repeating: UInt16(42))
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}

suite.test("SIMD64.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  var c = SIMD64(repeating: UInt16(42))
  c[0] = 1
  let storage = c.storage
  expectEqual(storage.count, c.indices.count)
  for i in c.indices {
    expectEqual(storage[i], c[i])
  }
}
