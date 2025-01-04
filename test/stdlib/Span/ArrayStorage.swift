//===--- ArrayStorage.swift --- storage properties for the Array family ---===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature Span) -enable-experimental-feature Span

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("ArrayStorageProperty")
defer { runAllTests() }

suite.test("Array.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  let storage = a.storage
  expectEqual(storage.count, capacity)
  expectEqual(storage[0], a[0])
}

suite.test("ContiguousArray.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  let capacity = 4
  let a = ContiguousArray(0..<capacity)
  let storage = a.storage
  expectEqual(storage.count, capacity)
  expectEqual(storage[0], a[0])
}

suite.test("ArraySlice.storage property")
.skip(.custom(
  { if #available(SwiftStdlib 6.1, *) { false } else { true } },
  reason: "Requires Swift 6.1's standard library"
))
.code {
  guard #available(SwiftStdlib 6.1, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  let storage1 = a.storage
  expectEqual(storage1.count, capacity)
  expectEqual(storage1[0], a[0])

  let s = a[...]
  let storage2 = s.storage
  expectEqual(storage2.count, capacity)
  expectEqual(storage2[0], a[0])

  let i1 = storage1.withUnsafeBufferPointer { Int(bitPattern: $0.baseAddress) }
  let i2 = storage1.withUnsafeBufferPointer { Int(bitPattern: $0.baseAddress) }
  expectEqual(i1, i2)
}
