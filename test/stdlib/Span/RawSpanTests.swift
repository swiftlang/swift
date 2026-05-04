//===--- RawSpanTests.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2026 Apple Inc. and the Swift project authors
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
    let intSpan = unsafe Span(_unsafeElements: $0)
    var span = unsafe RawSpan(unsafeElements: intSpan)
    expectEqual(span.byteCount, capacity*MemoryLayout<Int>.stride)
    expectFalse(span.isEmpty)

    span = RawSpan()
    expectTrue(span.isEmpty)
  }

  let a: [Int] = []
  a.withUnsafeBufferPointer {
    let intSpan = unsafe Span(_unsafeElements: $0)
    let span = unsafe RawSpan(unsafeElements: intSpan)
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

    let suffix = span.extracting(droppingFirst: 2)
    let u0 = suffix.unsafeLoadUnaligned(as: UInt64.self)
    expectEqual(u0.littleEndian & 0xff, 2)
    expectEqual(u0.bigEndian & 0xff, 9)
    let u1 = span.unsafeLoadUnaligned(fromByteOffset: 7, as: Int32.self)
    expectEqual(u1.littleEndian & 0xff, 7)
    expectEqual(u1.bigEndian & 0xff, 10)
  }
}

suite.test("extracting() functions")
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
    let sub1 = span.extracting(0..<2)
    let sub2 = span.extracting(..<2)
    let sub3 = span.extracting(...)
    let sub4 = span.extracting(2...)

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

suite.test("extracting(unchecked:) functions")
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
    let prefix = span.extracting(0..<8)
    let beyond = prefix.extracting(unchecked: 16...23)
    expectEqual(beyond.byteCount, 8)
    expectEqual(beyond.unsafeLoad(as: UInt8.self), 16)
  }
}

suite.test("prefix extracting() functions")
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
    expectEqual(span.extracting(first: 1).unsafeLoad(as: UInt8.self), 0)
    expectEqual(
      span.extracting(first: capacity).unsafeLoad(
        fromByteOffset: capacity-1, as: UInt8.self
      ),
      UInt8(capacity-1)
    )
    expectTrue(span.extracting(droppingLast: capacity).isEmpty)
    expectEqual(
      span.extracting(droppingLast: 1).unsafeLoad(
        fromByteOffset: capacity-2, as: UInt8.self
      ),
      UInt8(capacity-2)
    )
    let emptySpan = span.extracting(first: 0)
    let emptierSpan = emptySpan.extracting(0..<0)
    expectTrue(emptySpan.isIdentical(to: emptierSpan))
  }

  do {
    let b = UnsafeRawBufferPointer(start: nil, count: 0)
    let span = RawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span.extracting(first: 1).byteCount, b.count)
    expectEqual(span.extracting(droppingLast: 1).byteCount, b.count)
  }
}

suite.test("suffix extracting() functions")
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
    expectEqual(span.extracting(last: capacity).unsafeLoad(as: UInt8.self), 0)
    expectEqual(span.extracting(last: capacity-1).unsafeLoad(as: UInt8.self), 1)
    expectEqual(span.extracting(last: 1).unsafeLoad(as: UInt8.self), UInt8(capacity-1))
    expectTrue(span.extracting(droppingFirst: capacity).isEmpty)
    expectEqual(span.extracting(droppingFirst: 1).unsafeLoad(as: UInt8.self), 1)
  }

  do {
    let b = UnsafeRawBufferPointer(start: nil, count: 0)
    let span = RawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span.extracting(last: 1).byteCount, b.count)
    expectEqual(span.extracting(droppingFirst: 1).byteCount, b.count)
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
    let intSpan = unsafe Span(_unsafeElements: $0)
    let span = unsafe RawSpan(unsafeElements: intSpan)
    array.withUnsafeBytes {  b1 in
      span.withUnsafeBytes { b2 in
        expectTrue(b1.elementsEqual(b2))
      }
    }

    let emptyBuffer = UnsafeBufferPointer(rebasing: $0[0..<0])
    expectEqual(emptyBuffer.baseAddress, $0.baseAddress)

    let emptySpan = RawSpan(_unsafeElements: emptyBuffer)
    emptySpan.withUnsafeBytes {
      expectTrue($0.isEmpty)
      expectNotNil($0.baseAddress)
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
  let subSpan1 = span.extracting(first: 6)
  let subSpan2 = span.extracting(last: 6)
  let emptySpan = span.extracting(first: 0)
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

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

suite.test("RawSpan Sendability")
.require(.stdlib_6_2).code {
  let buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: 1, alignment: 2)
  defer { buffer.deallocate() }

  let span = RawSpan(_unsafeBytes: buffer)
  send(span)
}

suite.test("RawSpan safe loading")
.require(.stdlib_6_4).code {

  let array = ContiguousArray(repeating: UInt8.zero, count: 64)
  let bytes = array.span.bytes

  let u8 = bytes.load(fromByteOffset: 0, as: UInt8.self)
  expectEqual(u8, 0)

  let i8 = bytes.load(fromByteOffset: 0, as: Int8.self)
  expectEqual(i8, 0)

  let i64 = bytes.load(fromByteOffset: 37, as: Int64.self)
  expectEqual(i64, 0)
}

suite.test("RawSpan safe loading bounds underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_4).code {

  let array = ContiguousArray(repeating: UInt8.zero, count: 64)
  let bytes = array.span.bytes

  expectCrashLater()
  _ = bytes.load(fromByteOffset: -1, as: Int64.self)
}

suite.test("RawSpan safe loading bounds overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_4).code {

  let array = ContiguousArray(repeating: UInt8.zero, count: 64)
  let bytes = array.span.bytes

  expectCrashLater()
  _ = bytes.load(fromByteOffset: 59, as: Int64.self)
}

suite.test("RawSpan safe loading with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var array = ContiguousArray<UInt8>(repeating: 0, count: 8)
  array[0] = 0x01
  array[1] = 0x02
  let bytes = array.span.bytes

  let big = bytes.load(fromByteOffset: 0, as: UInt16.self, .bigEndian)
  expectEqual(big, 0x0102)

  let little = bytes.load(fromByteOffset: 0, as: UInt16.self, .littleEndian)
  expectEqual(little, 0x0201)

  let native = bytes.load(fromByteOffset: 0, as: UInt16.self, .native)
  let unqualified = bytes.load(fromByteOffset: 0, as: UInt16.self)
  expectEqual(native, unqualified)
}

suite.test("RawSpan subscript")
.require(.stdlib_6_4).code {
  let array: ContiguousArray<UInt8> = [10, 20, 30, 40]
  let bytes = array.span.bytes
  expectEqual(bytes[0], 10)
  expectEqual(bytes[1], 20)
  expectEqual(bytes[3], 40)
}

suite.test("RawSpan subscript bounds underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_4).code {
  let array: ContiguousArray<UInt8> = [10, 20, 30, 40]
  let bytes = array.span.bytes

  expectCrashLater()
  _ = bytes[-1]
}

suite.test("RawSpan subscript bounds overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_4).code {
  let array: ContiguousArray<UInt8> = [10, 20, 30, 40]
  let bytes = array.span.bytes

  expectCrashLater()
  _ = bytes[4]
}

suite.test("RawSpan unchecked subscript")
.require(.stdlib_6_4).code {
  let array: ContiguousArray<UInt8> = [10, 20, 30, 40]
  let bytes = array.span.bytes.extracting(first: 3)
  expectEqual(unsafe bytes[unchecked: 0], 10)
  expectEqual(unsafe bytes[unchecked: 3], 40)
}

suite.test("RawSpan init(elements:)")
.require(.stdlib_6_4).code {
  let capacity = 4
  let array = ContiguousArray(0..<capacity)
  let bytes = array.span.bytes
  expectEqual(bytes.byteCount, capacity * MemoryLayout<Int>.stride)
}

suite.test("Typed Span")
.require(.stdlib_6_4).code {
  let array = ContiguousArray(repeating: UInt64.zero, count: 64)
  let bytes = array.span.bytes

  let su8 = Span<UInt8>(viewing: bytes)
  expectEqual(su8[0], 0)

  let si8 = Span<Int8>(viewing: bytes)
  expectEqual(si8[0], 0)

  let i8 = bytes.load(fromByteOffset: 0, as: Int8.self)
  expectEqual(i8, 0)

  let i64 = bytes.load(fromByteOffset: 59, as: Int64.self)
  expectEqual(i64, 0)
}
