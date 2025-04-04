//===--- InlineSpanProperties.swift ---------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature AddressableTypes -enable-experimental-feature LifetimeDependence -enable-experimental-feature ValueGenerics)

// REQUIRES: executable_test
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_ValueGenerics

import StdlibUnittest

var suite = TestSuite("Span properties backed by inline storage")
defer { runAllTests() }

suite.test("CollectionOfOne.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var s = "A long string that is absolutely not smol at all."
  let u = Array(s.utf8)
  let c = CollectionOfOne(consume s)
  s = ""
  let span = c.span
  expectEqual(span.count, 1)
  let v = Array(span[0].utf8)
  expectEqual(u, v)
}

suite.test("CollectionOfOne.span property (simple)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  let c = CollectionOfOne(Int.random(in: 0..<100000))
  let span = c.span
  expectEqual(span.count, c.indices.count)
  expectEqual(span[0], c[0])
}

struct Padded: BitwiseCopyable {
  var storage: (Int64, Int8)
}

suite.test("CollectionOfOne.span stride test")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = CollectionOfOne(Padded(storage: (-1, 1)))
  let span = c.span
  let bytes = span.bytes
  expectEqual(bytes.byteCount, MemoryLayout.size(ofValue: c))
}

suite.test("CollectionOfOne.mutableSpan property (simple)")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var c = CollectionOfOne(Int.random(in: 0..<100000))
  expectEqual(c.count, 1)
  var span = c.mutableSpan
  expectEqual(span.count, 1)
  span[0] = Int.random(in: .min..<0)
  let r = span[0]
  expectEqual(c[0], r)
}

suite.test("InlineArray.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var s = InlineArray<5, Int>(repeating: 0)
  s[3] = .random(in: 0..<1000)
  let span = s.span
  expectEqual(span.count, s.count)
  for i in s.indices {
    expectEqual(span[i], s[i])
  }
}

suite.test("InlineArray.span property (String)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var s = InlineArray<5, String>(repeating: "0")
  s[3] = String(Int.random(in: 0..<1000))
  let span = s.span
  expectEqual(span.count, s.count)
  for i in s.indices {
    expectEqual(span[i], s[i])
  }    
}

suite.test("InlineArray.mutableSpan property")
.require(.stdlib_6_2).code
{
  guard #available(SwiftStdlib 6.2, *) else { return }

  var v = InlineArray<5, Int>(repeating: 0)
  let c = v.count
  var span = v.mutableSpan
  expectEqual(span.count, c)
  span[3] = Int.random(in: .min..<0)
  let r = span[3]
  expectEqual(v[3], r)
}

suite.test("InlineArray.mutableSpan property (String)")
.require(.stdlib_6_2).code
{
  guard #available(SwiftStdlib 6.2, *) else { return }

  var v = InlineArray<5, String>(repeating: "0")
  let c = v.count
  var span = v.mutableSpan
  expectEqual(span.count, c)
  span[3] = String(repeating: "0", count: Int.random(in: 100..<500))
  let s = span[3]
  expectTrue(s._isIdentical(to: v[3]))
}
