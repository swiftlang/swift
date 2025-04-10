//===--- SpanBytesProperty.swift --- test Span's bytes property -----------===//
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

import Swift
import StdlibUnittest

var suite = TestSuite("SpanBytesProperty")
defer { runAllTests() }

suite.test("Span/bytes property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = ContiguousArray(0..<capacity)
  a.withUnsafeBufferPointer {
    let bytes = $0.span.bytes

    expectEqual(bytes.byteCount, capacity*MemoryLayout<Int>.stride)
    expectEqual(bytes.unsafeLoad(fromByteOffset: 3, as: UInt8.self), 0)
  }
}
