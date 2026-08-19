//===--- AliasedSpanTests.swift -------------------------------------------===//
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

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("AliasedSpan Tests")
defer { runAllTests() }

suite.test("Initialize with ordinary element")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedSpan(_unsafeElements: $0)
    expectEqual(span.count, capacity)
    expectFalse(span.isEmpty)
    for i in span.indices {
      expectEqual(span[i], i)
    }
  }
}

suite.test("isEmpty")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  expectTrue(AliasedSpan<Int>().isEmpty)
  expectEqual(AliasedSpan<Int>().count, 0)
}

suite.test("extracting sub-spans")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 8
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedSpan(_unsafeElements: $0)

    let prefix = span.extracting(first: 3)
    expectEqual(prefix.count, 3)
    expectEqual(prefix[0], 0)
    expectEqual(prefix[2], 2)

    let suffix = span.extracting(last: 3)
    expectEqual(suffix.count, 3)
    expectEqual(suffix[0], 5)

    let middle = span.extracting(2..<5)
    expectEqual(middle.count, 3)
    expectEqual(middle[0], 2)

    expectEqual(span.extracting(droppingFirst: 2).count, 6)
    expectEqual(span.extracting(droppingLast: 2).count, 6)
    expectEqual(span.extracting(...).count, capacity)
    expectEqual(span.extracting(3...).count, 5)

    expectEqual(span.indices(of: middle), 2..<5)
    expectTrue(span.extracting(...).isIdentical(to: span))
  }
}

suite.test("Span <-> AliasedSpan round trip")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a = [1, 2, 3, 4, 5]
  a.withUnsafeBufferPointer {
    let span = unsafe Span(_unsafeElements: $0)
    let aliased = span.aliased
    expectEqual(aliased.count, span.count)
    expectEqual(aliased[2], 3)

    let backAgain = unsafe aliased.span
    expectEqual(backAgain.count, span.count)
    expectTrue(backAgain.isIdentical(to: span))
  }
}

suite.test("AliasedSpan iteration")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a = [10, 20, 30, 40]
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedSpan(_unsafeElements: $0)
    expectEqual(span.underestimatedCount, 4)

    var seen: [Int] = []
    var iterator = span.makeBorrowingIterator()
    while true {
      let chunk = iterator.nextSpan()
      if chunk.isEmpty { break }
      for i in chunk.indices { seen.append(chunk[i]) }
    }
    expectEqual(seen, a)
  }
}

suite.test("AliasedSpan iterator skip")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a = [10, 20, 30, 40]
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedSpan(_unsafeElements: $0)
    var iterator = span.makeBorrowingIterator()
    expectEqual(iterator.skip(by: 2), 2)
    do {
      let chunk = iterator.nextSpan()
      expectEqual(chunk.count, 1)
      expectEqual(chunk[0], 30)
    }
    expectEqual(iterator.skip(by: 10), 1)
    expectTrue(iterator.nextSpan().isEmpty)
  }
}

suite.test("AliasedMutableSpan element mutation")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableSpan(_unsafeElements: $0)
    expectEqual(span.count, 4)

    // The setter is non-mutating: it works on a `let` binding.
    span[0] = 100
    span[3] = 400
    expectEqual(span[0], 100)
    expectEqual(span[3], 400)

    span.swapAt(0, 3)
    expectEqual(span[0], 400)
    expectEqual(span[3], 100)
  }
  expectEqual(a, [400, 2, 3, 100])
}

suite.test("AliasedMutableSpan update(repeating:)")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableSpan(_unsafeElements: $0)
    span.update(repeating: 7)
  }
  expectEqual(a, [7, 7, 7, 7])
}

suite.test("AliasedMutableSpan is copyable")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableSpan(_unsafeElements: $0)
    // Two aliases of the same storage, both usable.
    let copy = span
    span[0] = 10
    expectEqual(copy[0], 10)
    copy[1] = 20
    expectEqual(span[1], 20)
    expectTrue(span.isIdentical(to: copy))
  }
  expectEqual(a, [10, 20, 3, 4])
}

suite.test("MutableSpan.asAliased()")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let mutable = unsafe MutableSpan(_unsafeElements: $0)
    let aliased = mutable.asAliased()
    aliased[2] = 30
    expectEqual(aliased.aliased[2], 30)
  }
  expectEqual(a, [1, 2, 30, 4])
}

suite.test("AliasedRawSpan basics")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a: [UInt32] = [0x0000_0001, 0x0000_0002]
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedRawSpan(_unsafeElements: $0)
    expectEqual(span.byteCount, 8)
    expectFalse(span.isEmpty)
    expectEqual(span.byteOffsets, 0..<8)

    expectEqual(span.load(fromByteOffset: 0, as: UInt32.self), 1)
    expectEqual(span.load(fromByteOffset: 4, as: UInt32.self), 2)

    let tail = span.extracting(droppingFirst: 4)
    expectEqual(tail.byteCount, 4)
    expectEqual(span.byteOffsets(of: tail), 4..<8)
  }
}

suite.test("AliasedMutableRawSpan store and load")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a: [UInt32] = [0, 0, 0, 0]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableRawSpan(_unsafeElements: $0)
    expectEqual(span.byteCount, 16)

    // Non-mutating stores on a `let` binding.
    span.storeBytes(of: UInt32(0xDEAD), toByteOffset: 0, as: UInt32.self)
    span.storeBytes(of: UInt32(0xBEEF), toByteOffset: 4, as: UInt32.self)
    expectEqual(span.load(fromByteOffset: 0, as: UInt32.self), 0xDEAD)
    expectEqual(span.load(fromByteOffset: 4, as: UInt32.self), 0xBEEF)

    span[8] = 0xFF
    expectEqual(span[8], 0xFF)
  }
  expectEqual(a[0], 0xDEAD)
  expectEqual(a[1], 0xBEEF)
}

suite.test("AliasedMutableRawSpan byte order")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a: [UInt8] = Array(repeating: 0, count: 4)
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableRawSpan(_unsafeElements: $0)
    span.storeBytes(
      of: UInt32(0x0102_0304), toByteOffset: 0, as: UInt32.self, .bigEndian
    )
    expectEqual(
      span.load(fromByteOffset: 0, as: UInt32.self, .bigEndian), 0x0102_0304
    )
  }
  expectEqual(a, [0x01, 0x02, 0x03, 0x04])
}

suite.test("Raw span conversions")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a: [UInt32] = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let raw = unsafe RawSpan(_unsafeElements: $0)
    let aliasedRaw = raw.aliased
    expectEqual(aliasedRaw.byteCount, raw.byteCount)
    expectTrue(unsafe aliasedRaw.rawSpan.isIdentical(to: raw))

    let mutableRaw = unsafe MutableRawSpan(_unsafeElements: $0)
    let aliasedMutableRaw = mutableRaw.asAliased()
    expectEqual(aliasedMutableRaw.byteCount, 16)
    expectEqual(aliasedMutableRaw.bytes.byteCount, 16)
  }
}

suite.test("Typed <-> raw aliased spans")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a: [UInt32] = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableSpan(_unsafeElements: $0)
    let bytes = span.mutableBytes
    expectEqual(bytes.byteCount, 16)

    let typed = AliasedMutableSpan<UInt32>(mutableBytes: bytes)
    expectEqual(typed.count, 4)
    typed[1] = 20
  }
  expectEqual(a, [1, 20, 3, 4])
}

suite.test("AliasedSpan(viewing:) an AliasedRawSpan")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a: [UInt32] = [1, 2, 3, 4]
  a.withUnsafeBufferPointer {
    let raw = unsafe AliasedRawSpan(_unsafeElements: $0)
    let typed = AliasedSpan<UInt32>(viewing: raw)
    expectEqual(typed.count, 4)
    expectEqual(typed[3], 4)

    let asBytes = typed.bytes
    expectEqual(asBytes.byteCount, 16)
  }
}

suite.test("withUnsafeBufferPointer access hatches")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var a = [1, 2, 3, 4]
  a.withUnsafeMutableBufferPointer {
    let span = unsafe AliasedMutableSpan(_unsafeElements: $0)
    let sum = span.withUnsafeBufferPointer { buffer in
      unsafe buffer.reduce(0, +)
    }
    expectEqual(sum, 10)

    span.withUnsafeMutableBufferPointer { buffer in
      for i in buffer.indices { unsafe buffer[i] *= 2 }
    }
  }
  expectEqual(a, [2, 4, 6, 8])
}

suite.test("AliasedRef basics")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let x = 42
  let ref = AliasedRef(x)
  expectEqual(ref.value, 42)
}

suite.test("AliasedMutableRef basics")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var x = 42
  do {
    let ref = AliasedMutableRef(&x)
    expectEqual(ref.value, 42)
    // The setter is non-mutating.
    ref.value = 100
    expectEqual(ref.value, 100)

    // `AliasedMutableRef` is copyable: two aliases of the same storage.
    let copy = ref
    copy.value = 200
    expectEqual(ref.value, 200)
    expectEqual(ref.aliased.value, 200)
  }
  expectEqual(x, 200)
}

suite.test("MutableRef.asAliased()")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var x = 1
  do {
    let mutableRef = MutableRef(&x)
    let aliased = mutableRef.asAliased()
    aliased.value = 7
    expectEqual(aliased.value, 7)
  }
  expectEqual(x, 7)
}

suite.test("Ref.aliased")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let x = 314
  let ref = Ref(x)
  expectEqual(ref.aliased.value, 314)
}

suite.test("Bounds checking traps")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let a = [1, 2, 3]
  a.withUnsafeBufferPointer {
    let span = unsafe AliasedSpan(_unsafeElements: $0)
    expectCrashLater()
    _ = span[3]
  }
}
