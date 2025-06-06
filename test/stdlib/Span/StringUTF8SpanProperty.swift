//===--- StringUTF8StorageProperty.swift ----------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableTypes

import StdlibUnittest

var suite = TestSuite("StringUTF8StorageProperty")
defer { runAllTests() }

suite.test("Span from Small String")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".utf8
  let u = Array(s)
  let span = s.span

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Large Native String")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A long string that is altogether not smol.".utf8
  let u = Array(s)
  let span = s.span

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Small String's Substring")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".dropFirst(8).utf8
  let u = Array("string.".utf8)
  let span = s.span

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Large Native String's Substring")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let t = "A long string that is altogether not smol."
  let s = t.dropFirst(22).prefix(10).utf8
  let u = Array("altogether".utf8)
  let span = s.span

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from String.utf8Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(200)
  let utf8span = s.utf8Span
  let span1 = utf8span.span
  let utf8view = s.utf8
  let span2 = utf8view.span
  expectEqual(span1.count, span2.count)
  for (i,j) in zip(span1.indices, span2.indices) {
    expectEqual(span1[i], span2[j])
  }
}

suite.test("UTF8Span from Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(200).utf8
  let span1 = s.span
  guard let utf8 = expectNotNil(try? UTF8Span(validating: span1)) else { return }

  expectEqual(utf8.count, span1.count)
  let span2 = utf8.span
  expectTrue(span1.isIdentical(to: span2))
  expectEqual(span1.count, span2.count)
  for (i,j) in zip(span1.indices, span2.indices) {
    expectEqual(span1[i], span2[j])
  }
}

suite.test("Span from Substring.utf8Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(22000).dropFirst().dropLast()
  let utf8span = s.utf8Span
  let span1 = utf8span.span
  let utf8view = s.utf8
  let span2 = utf8view.span
  expectEqual(span1.count, span2.count)
  for (i,j) in zip(span1.indices, span2.indices) {
    expectEqual(span1[i], span2[j])
  }
}
