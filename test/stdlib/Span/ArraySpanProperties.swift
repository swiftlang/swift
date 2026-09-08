//===--- ArraySpanProperties.swift ----------------------------------------===//
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

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("Array-Backed Span Properties")
defer { runAllTests() }

suite.test("Array.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  let span = a.span
  expectEqual(span.count, capacity)
  expectEqual(span[0], a[0])
}

suite.test("Array.mutableSpan property")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<capacity)
  var span = a.mutableSpan
  expectEqual(span.count, capacity)
  expectEqual(span[0], 0)
  span[0] = 100
  _ = consume span
  expectEqual(a[0], 100)
}

suite.test("ContiguousArray.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = ContiguousArray(0..<capacity)
  let span = a.span
  expectEqual(span.count, capacity)
  expectEqual(span[0], a[0])
}

suite.test("ContiguousArray.mutableSpan property")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = ContiguousArray(0..<capacity)
  var span = a.mutableSpan
  expectEqual(span.count, capacity)
  expectEqual(span[0], 0)
  span[0] = 100
  _ = consume span
  expectEqual(a[0], 100)
}

suite.test("ArraySlice.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  let span1 = a.span
  expectEqual(span1.count, capacity)
  expectEqual(span1[0], a[0])

  let s = a[...]
  let span2 = s.span
  expectEqual(span2.count, capacity)
  expectEqual(span2[0], a[0])

  let i1 = span1.withUnsafeBufferPointer { Int(bitPattern: $0.baseAddress) }
  let i2 = span1.withUnsafeBufferPointer { Int(bitPattern: $0.baseAddress) }
  expectEqual(i1, i2)
}

suite.test("KeyValuePairs.span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  let pairs = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
  let kvp: KeyValuePairs =  [1: "a", 2: "b", 3: "c", 4: "d"]
  
  let span = kvp.span
  expectEqual(span.count, kvp.count)
  for i in span.indices {
    expectEqual(span[i], pairs[i])
  }
}

suite.test("ArraySlice.mutableSpan property")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)

  var s = a[...]
  var span = s.mutableSpan
  expectEqual(span.count, capacity)
  expectEqual(span[0], a[0])

  span[0] += 100
  expectEqual(span[0], a[0]+100)

  _ = consume span
  expectEqual(s[0], a[0]+100)
}
