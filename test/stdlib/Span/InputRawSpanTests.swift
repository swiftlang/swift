//===--- InputRawSpanTests.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift(-strict-memory-safety)

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("InputRawSpan Tests")
defer { runAllTests() }

private struct Padded: BitwiseCopyable {
  var storage: (Int64, Int8)
}

// Runs `body` with an `InputRawSpan` over a zeroed buffer of `capacity` bytes,
// whose last `initialBytes.count` bytes are `initialBytes` and initialized.
@available(SwiftStdlib 6.5, *)
private func withInputRawSpan<R>(
  capacity: Int,
  initialBytes: [UInt8] = [],
  _ body: (inout InputRawSpan, UnsafeMutableRawBufferPointer) -> R
) -> R {
  let buffer = UnsafeMutableRawBufferPointer.allocate(
    byteCount: capacity, alignment: 16
  )
  defer { unsafe buffer.deallocate() }
  unsafe buffer.initializeMemory(as: UInt8.self, repeating: 0)
  let offset = capacity - initialBytes.count
  for (i, byte) in initialBytes.enumerated() {
    unsafe buffer[offset + i] = byte
  }
  var span = unsafe InputRawSpan(
    buffer: buffer, initializedCount: initialBytes.count
  )
  return unsafe body(&span, buffer)
}

suite.test("Create InputRawSpan")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var span = InputRawSpan()
  expectEqual(span.capacity, 0)
  expectEqual(span.byteCount, 0)
  expectTrue(span.isEmpty)
  expectTrue(span.isFull)
  expectEqual(span.bytes.byteCount, 0)

  span = unsafe InputRawSpan(
    buffer: UnsafeMutableRawBufferPointer(start: nil, count: 0),
    initializedCount: 0
  )
  expectEqual(span.capacity, 0)
  expectTrue(span.isEmpty)
  expectTrue(span.isFull)
  expectEqual(span.bytes.byteCount, 0)

  let c = 48
  let allocation = UnsafeMutableRawBufferPointer.allocate(
    byteCount: c, alignment: 16
  )
  defer { unsafe allocation.deallocate() }

  span = unsafe InputRawSpan(buffer: allocation, initializedCount: 0)
  expectEqual(span.capacity, c)
  expectEqual(span.freeCapacity, c)
  expectTrue(span.isEmpty)
  expectFalse(span.isFull)
  var remaining = unsafe span.finalize(for: allocation)
  expectEqual(remaining, 0)

  // exercise the initializer from a slice
  let slice = unsafe allocation[2..<8]
  for i in 0..<6 {
    unsafe allocation[2 + i] = UInt8(i)
  }
  span = unsafe InputRawSpan(buffer: slice, initializedCount: 6)
  expectEqual(span.capacity, 6)
  expectTrue(span.isFull)
  expectEqual(span.removeFirst(), 0)
  remaining = unsafe span.finalize(for: slice)
  expectEqual(remaining, 5)
}

suite.test("init with invalid initializedCount")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableRawBufferPointer.allocate(byteCount: 4, alignment: 16)
  defer { unsafe b.deallocate() }

  expectCrashLater(withDebugMessage: "count is invalid or greater than capacity")
  _ = unsafe InputRawSpan(buffer: b, initializedCount: 5)
}

suite.test("basic properties")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let c = 12
  let initialized = c / 4
  unsafe withInputRawSpan(
    capacity: c, initialBytes: Array(repeating: 7, count: initialized)
  ) { span, _ in
    expectEqual(span.capacity, c)
    expectEqual(span.byteCount, initialized)
    expectEqual(span.freeCapacity, c - initialized)
    expectFalse(span.isEmpty)
    expectFalse(span.isFull)
    expectEqual(span.byteOffsets, 0..<initialized)

    while !span.isFull { span.prepend(1) }
    expectEqual(span.freeCapacity, 0)
    expectFalse(span.isEmpty)
    expectTrue(span.isFull)
    expectEqual(span.byteOffsets, 0..<c)

    while !span.isEmpty { span.removeFirst() }
    expectEqual(span.freeCapacity, span.capacity)
    expectTrue(span.isEmpty)
    expectFalse(span.isFull)
    expectEqual(span.byteOffsets, 0..<0)
  }
}

suite.test("prepend and removeFirst")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 8) { span, buffer in
    span.prepend(1)
    span.prepend(2)
    span.prepend(3)
    expectEqual(span.byteCount, 3)
    // the initialized bytes are at the end of the buffer,
    // with the most recently prepended byte first
    expectEqual(unsafe buffer[5], 3)
    expectEqual(unsafe buffer[6], 2)
    expectEqual(unsafe buffer[7], 1)
    expectEqual(span[0], 3)
    expectEqual(span[2], 1)

    expectEqual(span.removeFirst(), 3)
    expectEqual(span.byteCount, 2)
    expectEqual(span.popFirst(), 2)
    expectEqual(span.popFirst(), 1)
    expectNil(span.popFirst())
    expectTrue(span.isEmpty)
  }
}

suite.test("prepend overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 1) { span, _ in
    span.prepend(1)
    expectCrashLater(withDebugMessage: "InputRawSpan capacity overflow")
    span.prepend(2)
  }
}

suite.test("removeFirst(_:)")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(
    capacity: 16, initialBytes: [0, 1, 2, 3, 4, 5, 6, 7]
  ) { span, _ in
    expectEqual(span.byteCount, 8)
    span.removeFirst(3)
    expectEqual(span.byteCount, 5)
    expectEqual(span[0], 3)
    span.removeFirst(0)
    expectEqual(span.byteCount, 5)
    span.removeFirst(5)
    expectTrue(span.isEmpty)
  }
}

suite.test("removeFirst on an empty span")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 4) { span, _ in
    expectCrashLater(withDebugMessage: "InputRawSpan underflow")
    span.removeFirst()
  }
}

suite.test("removeFirst(_:) with too many bytes")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 4, initialBytes: [1]) { span, _ in
    expectCrashLater(withDebugMessage: "InputRawSpan underflow")
    span.removeFirst(2)
  }
}

suite.test("removeFirst(_:) with a negative count")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 4, initialBytes: [1]) { span, _ in
    expectCrashLater(
      withDebugMessage: "Can't remove a negative number of bytes"
    )
    span.removeFirst(-1)
  }
}

suite.test("removeAll")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 8, initialBytes: [1, 2, 3]) { span, _ in
    span.removeAll()
    expectTrue(span.isEmpty)
    expectEqual(span.freeCapacity, 8)
    expectEqual(span.byteCount, 0)
  }
}

suite.test("prepend one unsafe")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let p = Padded(storage: (42, 7))
  unsafe withInputRawSpan(capacity: 64) { span, _ in
    unsafe span.prepend(paddedBytes: p, as: Padded.self)
    expectEqual(span.byteCount, MemoryLayout<Padded>.size)

    let v = unsafe span.bytes.unsafeLoadUnaligned(as: Padded.self)
    expectEqual(v.storage.0, 42)
    expectEqual(v.storage.1, 7)
  }
}

suite.test("prepend one safe")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 64) { span, _ in
    span.prepend(200_000, as: UInt32.self)
    span.prepend(400_000_000_000, as: UInt64.self)
    let total = MemoryLayout<UInt32>.size + MemoryLayout<UInt64>.size
    expectEqual(span.byteCount, total)

    // the last value prepended is first
    let u64 = span.bytes.load(fromByteOffset: 0, as: UInt64.self)
    expectEqual(u64, UInt64(4e11))
    let u32 = span.bytes.load(fromByteOffset: 8, as: UInt32.self)
    expectEqual(u32, 200_000)
  }
}

suite.test("prepend one with ByteOrder")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16) { span, _ in
    span.prepend(UInt16(0x0102), as: UInt16.self, .bigEndian)
    expectEqual(span[0], 0x01)
    expectEqual(span[1], 0x02)

    span.prepend(UInt16(0x0304), as: UInt16.self, .littleEndian)
    expectEqual(span[0], 0x04)
    expectEqual(span[1], 0x03)
    expectEqual(span[2], 0x01)
    expectEqual(span[3], 0x02)
  }
}

suite.test("prepend repeating unsafe")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let count = 3
  let p = Padded(storage: (99, -1))
  unsafe withInputRawSpan(capacity: count * MemoryLayout<Padded>.stride) { span, _ in
    unsafe span.prepend(
      repeatingPaddedBytes: p, count: count, as: Padded.self
    )
    expectEqual(span.byteCount, count * MemoryLayout<Padded>.stride)

    for i in 0..<count {
      let v = unsafe span.bytes.unsafeLoad(
        fromByteOffset: i * MemoryLayout<Padded>.stride, as: Padded.self
      )
      expectEqual(v.storage.0, 99)
      expectEqual(v.storage.1, -1)
    }
  }
}

suite.test("prepend repeating safe")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 64, initialBytes: [0xee]) { span, _ in
    span.prepend(repeating: 0xabcd, count: 4, as: UInt16.self)
    expectEqual(span.byteCount, 4 * MemoryLayout<UInt16>.stride + 1)

    for i in 0..<4 {
      let loaded = span.bytes.load(
        fromByteOffset: i * MemoryLayout<UInt16>.stride, as: UInt16.self
      )
      expectEqual(loaded, 0xabcd)
    }
    // the byte that was already there is still last
    expectEqual(span[8], 0xee)
  }
}

suite.test("prepend repeating negative count")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16) { span, _ in
    expectCrashLater(
      withDebugMessage: "Can't prepend a negative number of values"
    )
    span.prepend(repeating: 0, count: -1, as: UInt8.self)
  }
}

suite.test("prepend repeating overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16) { span, _ in
    span.prepend(0)
    expectCrashLater(withDebugMessage: "InputRawSpan capacity overflow")
    span.prepend(repeating: 1, count: 8, as: UInt16.self)
  }
}

suite.test("prepend repeating with ByteOrder")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16) { span, _ in
    span.prepend(repeating: 0x0102, count: 4, as: UInt16.self, .bigEndian)
    for i in 0..<4 {
      let offset = i * MemoryLayout<UInt16>.stride
      expectEqual(span[offset],     0x01)
      expectEqual(span[offset + 1], 0x02)
    }

    span.removeAll()
    span.prepend(repeating: 0xabcd, count: 4, as: UInt16.self, .littleEndian)
    for i in 0..<4 {
      let offset = i * MemoryLayout<UInt16>.stride
      expectEqual(span[offset],     0xcd)
      expectEqual(span[offset + 1], 0xab)
    }
  }
}

suite.test("subscript getter and setter")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16, initialBytes: [0, 10, 20, 30]) { span, buffer in
    expectEqual(span[0], 0)
    expectEqual(span[1], 10)
    expectEqual(span[2], 20)
    expectEqual(span[3], 30)

    span[0] = 0xaa
    span[3] = 0xbb
    expectEqual(span[0], 0xaa)
    expectEqual(span[3], 0xbb)
    expectEqual(unsafe buffer[12], 0xaa)
    expectEqual(unsafe buffer[15], 0xbb)
  }
}

suite.test("subscript bounds underflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16, initialBytes: [1]) { span, _ in
    expectCrashLater(withDebugMessage: "Index out of bounds")
    _ = span[-1]
  }
}

suite.test("subscript bounds overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16, initialBytes: [1]) { span, _ in
    expectCrashLater(withDebugMessage: "Index out of bounds")
    _ = span[1]
  }
}

suite.test("unchecked subscript getter and setter")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16, initialBytes: [0, 10, 20, 30]) { span, _ in
    expectEqual(unsafe span[unchecked: 0], 0)
    expectEqual(unsafe span[unchecked: 3], 30)

    unsafe span[unchecked: 0] = 0xcc
    unsafe span[unchecked: 1] = 0xdd
    expectEqual(unsafe span[unchecked: 0], 0xcc)
    expectEqual(unsafe span[unchecked: 1], 0xdd)
    expectEqual(span[1], 0xdd)
  }
}

suite.test("bytes and mutableBytes properties")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 16, initialBytes: [1, 2, 3, 4]) { span, _ in
    expectEqual(span.bytes.byteCount, 4)
    expectEqual(unsafe span.bytes.unsafeLoad(
      fromByteOffset: 3, as: UInt8.self
    ), 4)

    do {
      var mb = span.mutableBytes
      expectEqual(mb.byteCount, 4)
      mb.storeBytes(of: UInt8(99), toByteOffset: 0, as: UInt8.self)
    }
    expectEqual(span[0], 99)
    expectEqual(span[1], 2)
  }
}

suite.test("withUnsafeMutableBytes")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let c = 17
  unsafe withInputRawSpan(capacity: c, initialBytes: [1, 2, 3]) { span, _ in
    unsafe span.withUnsafeMutableBytes { buffer, count in
      expectEqual(buffer.count, c)
      expectEqual(count, 3)
      // fill the slot just before the initialized region
      unsafe buffer[c - 4] = 0xaa
      count += 1
    }
    expectEqual(span.byteCount, 4)
    expectEqual(span[0], 0xaa)
    expectEqual(span[1], 1)

    unsafe span.withUnsafeMutableBytes { _, count in
      count -= 3
    }
    expectEqual(span.byteCount, 1)
    expectEqual(span[0], 3)
  }
}

suite.test("withUnsafeMutableBytes capacity overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  unsafe withInputRawSpan(capacity: 4) { span, _ in
    expectCrashLater(withDebugMessage: "InputRawSpan capacity overflow")
    unsafe span.withUnsafeMutableBytes { _, count in
      count = 5
    }
  }
}

suite.test("finalize identity mismatch")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableRawBufferPointer.allocate(byteCount: 4, alignment: 16)
  defer { unsafe b.deallocate() }

  let span = unsafe InputRawSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "InputRawSpan identity mismatch")
  _ = unsafe span.finalize(for: b[1...])
}

suite.test("Iterable conformance")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let bytes: [UInt8] = [0, 1, 2, 3, 4]
  unsafe withInputRawSpan(capacity: 8, initialBytes: bytes) { span, _ in
    expectEqual(span.underestimatedCount, bytes.count)
    var iterator = span.makeBorrowingIterator()
    var collected: [UInt8] = []
    while true {
      let next = iterator.nextSpan(maxCount: 2)
      if next.isEmpty { break }
      expectLE(next.count, 2)
      for i in next.indices {
        collected.append(next[i])
      }
    }
    expectEqual(collected, bytes)
  }
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

suite.test("InputRawSpan Sendability")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  send(InputRawSpan())
}
