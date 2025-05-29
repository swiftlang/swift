//===--- RawSpanTests.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("Span Tests")
defer { runAllTests() }

suite.test("Initialize with Span<Int>")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  Array(0..<capacity).withUnsafeBufferPointer {
    let intSpan = Span(_unsafeElements: $0)
    let span = RawSpan(_elements: intSpan)
    expectEqual(span.byteCount, capacity*MemoryLayout<Int>.stride)
    expectFalse(span.isEmpty)
  }

  let a: [Int] = []
  a.withUnsafeBufferPointer {
    let intSpan = Span(_unsafeElements: $0)
    let span = RawSpan(_elements: intSpan)
    expectTrue(span.isEmpty)
  }
}

suite.test("Initialize with UnsafeRawBufferPointer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity*MemoryLayout<Int>.stride)
  }

  a.withUnsafeMutableBytes {
    let span = RawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity*MemoryLayout<Int>.stride)
  }
}

suite.test("Initialize with UnsafeRawPointer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeBytes {
    let pointer = $0.baseAddress!
    let span = RawSpan(
      _unsafeStart: pointer,
      byteCount: capacity*MemoryLayout<Int>.stride
    )
    expectEqual(span.byteCount, $0.count)
  }

  a.withUnsafeMutableBytes {
    let pointer = $0.baseAddress!
    let span = RawSpan(
      _unsafeStart: pointer,
      byteCount: capacity*MemoryLayout<Int>.stride
    )
    expectEqual(span.byteCount, $0.count)
  }
}

suite.test("unsafeLoad(as:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let s = (0..<capacity).map({ "\(#file)+\(#function) #\($0)" })
  s.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    let stride = MemoryLayout<String>.stride

    let s0 = span.unsafeLoad(as: String.self)
    expectEqual(s0.contains("0"), true)
    let s1 = span.unsafeLoad(fromByteOffset: stride, as: String.self)
    expectEqual(s1.contains("1"), true)
    let s2 = span.unsafeLoad(fromUncheckedByteOffset: 2*stride, as: String.self)
    expectEqual(s2.contains("2"), true)
  }
}

suite.test("unsafeLoadUnaligned(as:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 64
  let a = Array(0..<UInt8(capacity))
  a.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)

    let suffix = span._extracting(droppingFirst: 2)
    let u0 = suffix.unsafeLoadUnaligned(as: UInt64.self)
    expectEqual(u0.littleEndian & 0xff, 2)
    expectEqual(u0.bigEndian & 0xff, 9)
    let u1 = span.unsafeLoadUnaligned(fromByteOffset: 7, as: Int32.self)
    expectEqual(u1.littleEndian & 0xff, 7)
    expectEqual(u1.bigEndian & 0xff, 10)
  }
}

suite.test("_extracting() functions")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let b = (0..<capacity).map(Int8.init)
  b.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    let sub1 = span._extracting(0..<2)
    let sub2 = span._extracting(..<2)
    let sub3 = span._extracting(...)
    let sub4 = span._extracting(2...)

    sub1.withUnsafeBytes { p1 in
      sub2.withUnsafeBytes { p2 in
        expectTrue(p1.elementsEqual(p2))
      }
    }
    sub3.withUnsafeBytes { p3 in
      span.withUnsafeBytes { p0 in
        expectTrue(p3.elementsEqual(p0))
      }
    }
    sub4.withUnsafeBytes { p4 in
      sub3.withUnsafeBytes { p3 in
        expectFalse(p4.elementsEqual(p3))
      }
    }
  }
}

suite.test("_extracting(unchecked:) functions")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 32
  let b = (0..<capacity).map(UInt8.init)
  b.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    let prefix = span._extracting(0..<8)
    let beyond = prefix._extracting(unchecked: 16...23)
    expectEqual(beyond.byteCount, 8)
    expectEqual(beyond.unsafeLoad(as: UInt8.self), 16)
  }
}

suite.test("prefix _extracting() functions")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<UInt8(capacity))
  a.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity)
    expectEqual(span._extracting(first: 1).unsafeLoad(as: UInt8.self), 0)
    expectEqual(
      span._extracting(first: capacity).unsafeLoad(
        fromByteOffset: capacity-1, as: UInt8.self
      ),
      UInt8(capacity-1)
    )
    expectTrue(span._extracting(droppingLast: capacity).isEmpty)
    expectEqual(
      span._extracting(droppingLast: 1).unsafeLoad(
        fromByteOffset: capacity-2, as: UInt8.self
      ),
      UInt8(capacity-2)
    )
    let emptySpan = span._extracting(first: 0)
    let emptierSpan = emptySpan._extracting(0..<0)
    expectTrue(emptySpan.isIdentical(to: emptierSpan))
  }

  do {
    let b = UnsafeRawBufferPointer(start: nil, count: 0)
    let span = RawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span._extracting(first: 1).byteCount, b.count)
    expectEqual(span._extracting(droppingLast: 1).byteCount, b.count)
  }
}

suite.test("suffix _extracting() functions")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<UInt8(capacity))
  a.withUnsafeBytes {
    let span = RawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity)
    expectEqual(span._extracting(last: capacity).unsafeLoad(as: UInt8.self), 0)
    expectEqual(span._extracting(last: capacity-1).unsafeLoad(as: UInt8.self), 1)
    expectEqual(span._extracting(last: 1).unsafeLoad(as: UInt8.self), UInt8(capacity-1))
    expectTrue(span._extracting(droppingFirst: capacity).isEmpty)
    expectEqual(span._extracting(droppingFirst: 1).unsafeLoad(as: UInt8.self), 1)
  }

  do {
    let b = UnsafeRawBufferPointer(start: nil, count: 0)
    let span = RawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span._extracting(last: 1).byteCount, b.count)
    expectEqual(span._extracting(droppingFirst: 1).byteCount, b.count)
  }
}

suite.test("withUnsafeBytes()")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let array = Array(0..<capacity)
  array.withUnsafeBufferPointer {
    let intSpan = Span(_unsafeElements: $0)
    let span = RawSpan(_elements: intSpan)
    array.withUnsafeBytes {  b1 in
      span.withUnsafeBytes { b2 in
        expectTrue(b1.elementsEqual(b2))
      }
    }

    let emptyBuffer = UnsafeBufferPointer(rebasing: $0[0..<0])
    expectEqual(emptyBuffer.baseAddress, $0.baseAddress)

    let emptySpan = RawSpan(_unsafeElements: emptyBuffer)
    emptySpan.withUnsafeBytes {
      expectNil($0.baseAddress)
    }
  }
}

suite.test("byteOffsets(of:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableRawBufferPointer.allocate(byteCount: 8, alignment: 8)
  defer { b.deallocate() }

  let span = RawSpan(_unsafeBytes: b)
  let subSpan1 = span._extracting(first: 6)
  let subSpan2 = span._extracting(last: 6)
  let emptySpan = span._extracting(first: 0)
  let nilBuffer = UnsafeRawBufferPointer(start: nil, count: 0)
  let nilSpan = RawSpan(_unsafeBytes: nilBuffer)

  var bounds: Range<Int>?
  bounds = span.byteOffsets(of: subSpan1)
  expectEqual(bounds, span.byteOffsets.prefix(6))
  bounds = span.byteOffsets(of: subSpan2)
  expectEqual(bounds, span.byteOffsets.suffix(6))
  bounds = subSpan2.byteOffsets(of: subSpan1)
  expectNil(bounds)
  bounds = subSpan1.byteOffsets(of: subSpan2)
  expectNil(bounds)
  bounds = subSpan2.byteOffsets(of: span)
  expectNil(bounds)
  bounds = nilSpan.byteOffsets(of: emptySpan)
  expectNil(bounds)
  bounds = span.byteOffsets(of: nilSpan)
  expectNil(bounds)
  bounds = nilSpan.byteOffsets(of: nilSpan)
  expectEqual(bounds, 0..<0)
}
