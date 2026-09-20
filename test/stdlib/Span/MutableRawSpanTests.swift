//===--- MutableRawSpanTests.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

// Blocked by rdar://181604244 (opaque values borrow accessors)
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// Crash when passing a `Span` as a `borrowing some Iterable`
// https://github.com/swiftlang/swift/issues/92448
// XFAIL: swift_test_mode_optimize && !swift_stdlib_asserts
// XFAIL: swift_test_mode_optimize_size && !swift_stdlib_asserts

import StdlibUnittest

var suite = TestSuite("MutableRawSpan Tests")
defer { runAllTests() }

suite.test("Basic Initializer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var s = Array("\(#file)+\(#function)--\(Int.random(in: 1000...9999))".utf8)
  s.withUnsafeMutableBytes {
    var b = MutableRawSpan(_unsafeBytes: $0)
    expectEqual(b.byteCount, $0.count)
    expectFalse(b.isEmpty)
    expectEqual(b.byteOffsets, 0..<$0.count)

    b = MutableRawSpan()
    expectEqual(b.byteCount, 0)
  }
}

private struct Padded: BitwiseCopyable {
  var storage: (Int64, Int8)
}

suite.test("Initializer from MutableSpan")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var array = [0, 1, 2].map({ Padded(storage: (Int64($0), Int8($0))) })
  array.withUnsafeMutableBufferPointer {
    var span = MutableSpan(_unsafeElements: $0)
    var rawSpan = MutableRawSpan(_elements: &span)

    expectEqual(rawSpan.byteCount, $0.count * MemoryLayout<Padded>.stride)

    rawSpan.storeBytes(of: 15, as: Int64.self)
  }
  expectEqual(array[0].storage.0, 15)

  var slice = array.prefix(1)
  slice.withUnsafeMutableBufferPointer {
    expectEqual($0.count, 1)
    var span = MutableSpan(_unsafeElements: $0)
    var rawSpan = MutableRawSpan(_elements: &span)

    expectEqual(rawSpan.byteCount, MemoryLayout<Padded>.size)

    rawSpan.storeBytes(of: 3, as: Int64.self)
  }
  expectEqual(slice[0].storage.0, 3)
}

suite.test("isEmpty property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var array = [0, 1, 2]
  array.withUnsafeMutableBufferPointer {
    var span = MutableRawSpan(_unsafeElements: $0)
    expectFalse(span.isEmpty)

    let e = $0.extracting(0..<0)
    span = MutableRawSpan(_unsafeElements: e)
    expectTrue(span.isEmpty)
  }
}

suite.test("indices property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4 as UInt8
  var a = Array(0..<capacity)
  a.withUnsafeMutableBytes {
    let view = MutableRawSpan(_unsafeBytes: $0)
    expectEqual(view.byteCount, view.byteOffsets.count)
    expectTrue(view.byteOffsets.elementsEqual(0..<view.byteCount))
  }
}

suite.test("withUnsafeBytes()")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 64
  var a = Array(0..<capacity)
  let i = Int.random(in: a.indices)
  a.withUnsafeMutableBytes {
    var view = MutableRawSpan(_unsafeBytes: $0)
    view.withUnsafeBytes {
      expectEqual($0.load(fromByteOffset: i, as: UInt8.self), $0[i])
    }

    let empty = UnsafeMutableRawBufferPointer(start: $0.baseAddress, count: 0)
    view = MutableRawSpan(_unsafeBytes: empty)
    view.withUnsafeBytes {
      expectEqual($0.count, 0)
      expectNotNil($0.baseAddress)
    }
  }
}

suite.test("withUnsafeMutableBytes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 16
  var a = Array(0..<capacity)
  let i = Int.random(in: a.indices)
  a.withUnsafeMutableBytes {
    var view = MutableRawSpan(_unsafeBytes: $0)
    let o = view.unsafeLoad(fromByteOffset: i, as: UInt8.self)
    view.withUnsafeMutableBytes {
      $0.storeBytes(of: UInt8(i+1), toByteOffset: i, as: UInt8.self)
    }
    let m = view.unsafeLoad(fromByteOffset: i, as: UInt8.self)
    expectEqual(m, o+1)

    let empty = UnsafeMutableRawBufferPointer(start: $0.baseAddress, count: 0)
    view = MutableRawSpan(_unsafeBytes: empty)
    view.withUnsafeMutableBytes {
      expectEqual($0.count, 0)
      expectNotNil($0.baseAddress)
    }
  }
  expectEqual(Int(a[i]), i+1)
}

suite.test("bytes property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var array = [0, 1, 2]
  array.withUnsafeMutableBufferPointer {
    let mutable = MutableRawSpan(_unsafeElements: $0)
    let immutable = mutable.bytes
    expectEqual(mutable.byteCount, immutable.byteCount)
  }
}

suite.test("unsafeView(as:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var array = [0, 1, 2]
  array.withUnsafeMutableBufferPointer {
    let mutable = MutableRawSpan(_unsafeElements: $0)
    let view = mutable._unsafeView(as: Int.self)
    expectEqual($0[1], view[1])
  }
}

suite.test("unsafeMutableView(as:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var array = [0, 1, 2]
  let value = Int.random(in: 0..<1000)
  array.withUnsafeMutableBufferPointer {
    var mutable = MutableRawSpan(_unsafeElements: $0)
    var view = mutable._unsafeMutableView(as: Int.self)
    view[1] = value
  }
  expectEqual(array[1], value)
}

suite.test("unsafeLoad(as:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var s = (0..<capacity).map({ "\(#file)+\(#function) #\($0)" })
  s.withUnsafeMutableBytes {
    let span = MutableRawSpan(_unsafeBytes: $0)
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
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBytes {
    var span = MutableRawSpan(_unsafeBytes: $0)

    let suffix = span._mutatingExtracting(droppingFirst: 2)
    let u0 = suffix.unsafeLoadUnaligned(as: UInt64.self)
    expectEqual(u0 & 0xff, 2)
    expectEqual(u0.byteSwapped & 0xff, 9)
    let u1 = span.unsafeLoadUnaligned(fromByteOffset: 6, as: UInt64.self)
    expectEqual(u1 & 0xff, 6)
    let u3 = span.unsafeLoadUnaligned(fromUncheckedByteOffset: 7, as: UInt32.self)
    expectEqual(u3 & 0xff, 7)
  }
}

suite.test("storeBytes(of:as:)")
.require(.stdlib_6_2).code {
  let padded = Padded(storage: (8, 1))

  let count = 4
  var a = ContiguousArray(repeating: padded, count: count)
  var span = a.mutableSpan
  var bytes = span.mutableBytes

  bytes.storeBytes(of: .max, toByteOffset: 8,  as: Int8.self)
  bytes.storeBytes(of: .max, toByteOffset: 16, as: Int64.self)

  expectEqual(a[0].storage.1, .max)
  expectEqual(a[1].storage.0, .max)
}

suite.test("storeBytes(of:as:) safe")
.require(.stdlib_6_4).code {

  var array = ContiguousArray(repeating: UInt8.zero, count: 32)
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  bytes.storeBytes(of: 91, toByteOffset: 0, as: UInt8.self)
  bytes.storeBytes(of: 0x12345678, toByteOffset: 4, as: UInt32.self)
  bytes.storeBytes(of: .max, toByteOffset: 8, as: UInt64.self)
  expectEqual(bytes[0], 91)
  let u32 = bytes.load(fromByteOffset: 4, as: UInt32.self)
  expectEqual(u32, 0x12345678)
  let u64 = bytes.load(fromByteOffset: 8, as: UInt64.self)
  expectEqual(u64, .max)
}

suite.test("storeBytes(of:as:) unsafe")
.require(.stdlib_6_4).code {

  var array = ContiguousArray(
    repeating: UInt8.zero, count: MemoryLayout<Padded>.stride
  )
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  let p = Padded(storage: (42, 7))
  unsafe bytes.storeBytes(of: p, toByteOffset: 0, as: Padded.self)
  let result = unsafe bytes.unsafeLoad(as: Padded.self)
  expectEqual(result.storage.0, 42)
  expectEqual(result.storage.1, 7)
}

suite.test("storeBytes(repeating:count:as:) safe")
.require(.stdlib_6_4).code {

  var array = ContiguousArray(
    repeating: UInt8.zero, count: 8 * MemoryLayout<UInt16>.stride
  )
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  bytes.storeBytes(repeating: 19999, count: 8, as: UInt16.self)
  for i in 0..<8 {
    let v = bytes.load(
      fromByteOffset: i * MemoryLayout<UInt16>.stride, as: UInt16.self
    )
    expectEqual(v, 19999)
  }
}

suite.test("storeBytes(repeating:count:as:) count zero")
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt8>([0xaa, 0xbb])
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  bytes.storeBytes(repeating: .max, count: 0, as: UInt8.self)
  expectEqual(bytes[0], 0xaa)
  expectEqual(bytes[1], 0xbb)
}

suite.test("storeBytes(repeating:count:as:) negative count")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var array = ContiguousArray(repeating: UInt8.zero, count: 16)
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  expectCrashLater()
  bytes.storeBytes(repeating: .max, count: -1, as: UInt8.self)
}

suite.test("storeBytes(repeating:count:as:) unsafe")
.require(.stdlib_6_4).code {

  let count = 3
  var array = ContiguousArray(
    repeating: UInt8.zero, count: count * MemoryLayout<Padded>.stride
  )
  var span = array.mutableSpan
  var bytes = span.mutableBytes
  let p = Padded(storage: (99, -1))
  unsafe bytes.storeBytes(repeating: p, count: count, as: Padded.self)
  for i in 0..<count {
    let v = unsafe bytes.unsafeLoad(
      fromByteOffset: i * MemoryLayout<Padded>.stride, as: Padded.self
    )
    expectEqual(v.storage.0, 99)
    expectEqual(v.storage.1, -1)
  }
}

suite.test("updateAll(repeating:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  var a = ContiguousArray<UInt8>(repeating: 0, count: 8)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateAll(repeating: 0xa5)
  expectTrue(a.allSatisfy({ $0 == 0xa5 }))

  var empty = MutableRawSpan()
  empty.updateAll(repeating: .max)
  expectTrue(empty.isEmpty)
}

suite.test("updateSubrange(_:repeating:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  let byteCount = 8
  var a = ContiguousArray<UInt8>(repeating: 0, count: byteCount)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateSubrange(2..<6, repeating: 0xff)
  let i = Int.random(in: 0...span.byteCount)
  span.updateSubrange(i..<i, repeating: 0x11)
  expectEqual(a, [0, 0, 0xff, 0xff, 0xff, 0xff, 0, 0])
}

suite.test("updateSubrange(_:repeating:) bounds overflow")
.require(.minimumStdlib(.stdlib_6_5))
.require(.crashTesting)
.crashOutputMatches("Byte offset range out of bounds", when: _isDebugAssertConfiguration())
.code {
  var a = ContiguousArray([1, 2, 3, 4])
  var span = MutableRawSpan(elements: a.mutableSpan)
  expectCrashLater()
  span.updateSubrange(2 ..< .max, repeating: 0)
}

suite.test("updateAll(copying:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  let source = ContiguousArray<UInt8>([1, 2, 3, 4, 5, 6, 7, 8])
  var a = ContiguousArray<UInt8>(repeating: 0, count: source.count)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateAll(copying: source.span.bytes)
  expectEqual(a, source)

  var empty = MutableRawSpan()
  empty.updateAll(copying: RawSpan())
  expectTrue(empty.isEmpty)
}

suite.test("updateAll(copying:) count mismatch")
.require(.minimumStdlib(.stdlib_6_5))
.require(.crashTesting)
.code {
  let source = ContiguousArray<UInt8>([1, 2, 3])
  var a = ContiguousArray<UInt8>(repeating: 0, count: 4)
  var span = MutableRawSpan(elements: a.mutableSpan)
  expectCrashLater()
  span.updateAll(copying: source.span.bytes)
}

suite.test("updateSubrange(_:copying:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  let source = ContiguousArray<UInt8>([0xd, 0xe, 0xf])
  var a = ContiguousArray<UInt8>(repeating: 0, count: 8)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateSubrange(4..<7, copying: source.span.bytes)
  span.updateSubrange(0..<0, copying: RawSpan())
  expectEqual(a, [0, 0, 0, 0, 0xd, 0xe, 0xf, 0])
}

suite.test("updateSubrange(_:copying:) count mismatch")
.require(.minimumStdlib(.stdlib_6_5))
.require(.crashTesting)
.code {
  let source = ContiguousArray<UInt8>([1, 2, 3])
  var a = ContiguousArray<UInt8>(repeating: 0, count: 4)
  var span = MutableRawSpan(elements: a.mutableSpan)
  expectCrashLater()
  span.updateSubrange(0..<2, copying: source.span.bytes)
}

suite.test("updateAll(moving:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  let byteCount = 8
  let storage = UnsafeMutableRawBufferPointer.allocate(
    byteCount: byteCount, alignment: 8
  )
  defer { storage.deallocate() }
  _ = storage.initializeMemory(as: UInt8.self, fromContentsOf: 0..<8)
  var source = OutputRawSpan(buffer: storage, initializedCount: 8)
  expectEqual(source.isFull, true)

  var a = ContiguousArray<UInt8>(repeating: 0xff, count: byteCount)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateAll(moving: &source)
  expectEqual(a.elementsEqual(0..<UInt8(byteCount)), true)

  expectEqual(source.isEmpty, true)
  expectEqual(source.finalize(for: storage), 0)
}

suite.test("updateSubrange(_:moving:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  let storage = UnsafeMutableRawBufferPointer.allocate(
    byteCount: 2, alignment: 8
  )
  defer { storage.deallocate() }
  var source = OutputRawSpan(buffer: storage, initializedCount: 0)
  source.append(0xab)
  source.append(0xcd)
  expectEqual(source.isFull, true)

  var a = ContiguousArray<UInt8>(repeating: 0, count: 6)
  var span = MutableRawSpan(elements: a.mutableSpan)
  span.updateSubrange(2..<4, moving: &source)
  expectEqual(a, [0, 0, 0xab, 0xcd, 0, 0])

  expectEqual(source.isEmpty, true)
  expectEqual(source.finalize(for: storage), 0)
}

suite.test("updateFromIndex(_:copying:)")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  guard #available(SwiftStdlib 6.4, *) else { return } // for `Iterable`

  let byteCount = 8
  let source = ContiguousArray<UInt8>([1, 2, 3, 4])
  let more: [2 of UInt8] = [8, 9]

  var a = ContiguousArray<UInt8>(repeating: 0, count: byteCount)
  var span = MutableRawSpan(elements: a.mutableSpan)

  var end = span.updateFromIndex(2, copying: source.span)
  expectEqual(end, 6)
  end = span.updateFromIndex(end, copying: more)
  expectEqual(end, byteCount)

  end = span.updateFromIndex(byteCount, copying: RawSpan())
  expectEqual(end, byteCount)

  expectEqual(a, [0, 0, 1, 2, 3, 4, 8, 9])
}

suite.test("updateFromIndex(_:copying:), Iterable overflows source")
.require(.minimumStdlib(.stdlib_6_5))
.require(.crashTesting)
.code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let source = ContiguousArray<UInt8>(0..<8)
  var a = ContiguousArray<UInt8>(repeating: 0, count: 4)
  var span = MutableRawSpan(elements: a.mutableSpan)
  expectCrashLater()
  _ = span.updateFromIndex(0, copying: source.span)
}

suite.test("updateFromIndex(_:copying:), bad index")
.require(.minimumStdlib(.stdlib_6_5))
.require(.crashTesting)
.crashOutputMatches("Byte offset out of bounds", when: _isDebugAssertConfiguration())
.code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  var a = ContiguousArray<UInt8>(repeating: 0, count: 4)
  var span = MutableRawSpan(elements: a.mutableSpan)
  expectCrashLater()
  _ = span.updateFromIndex(5, copying: Span<UInt8>())
}

suite.test("updateFromIndex(_:copying:), from inout borrowing iterator")
.require(.minimumStdlib(.stdlib_6_5))
.code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let source = ContiguousArray<UInt8>(0..<6)
  let elements = source.span
  var iterator = elements.makeBorrowingIterator()

  var first = ContiguousArray<UInt8>(repeating: 0xff, count: 4)
  var span1 = MutableRawSpan(elements: first.mutableSpan)
  var offset = 0
  span1.updateFromIndex(&offset, copying: &iterator)
  expectEqual(offset, 4)
  expectEqual(first, [0, 1, 2, 3])

  var second = ContiguousArray<UInt8>(repeating: 0xff, count: 4)
  var span2 = MutableRawSpan(elements: second.mutableSpan)
  offset = 0
  span2.updateFromIndex(&offset, copying: &iterator)
  expectEqual(offset, 2)
  expectEqual(second, [4, 5, 0xff, 0xff])
}

suite.test("_mutatingExtracting()")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 4
  var b = (0..<capacity).map(Int8.init)
  b.withUnsafeMutableBytes {
    var span = MutableRawSpan(_unsafeBytes: $0)

    var sub = span._mutatingExtracting(0..<2)
    expectEqual(sub.byteCount, 2)
    expectEqual(sub.unsafeLoad(as: UInt8.self), 0)

    sub = span._mutatingExtracting(..<2)
    expectEqual(sub.byteCount, 2)
    expectEqual(sub.unsafeLoad(as: UInt8.self), 0)

    sub = span._mutatingExtracting(...)
    expectEqual(sub.byteCount, 4)
    expectEqual(sub.unsafeLoad(as: UInt8.self), 0)

    sub = span._mutatingExtracting(2...)
    expectEqual(sub.byteCount, 2)
    expectEqual(sub.unsafeLoad(as: UInt8.self), 2)

    sub = span._mutatingExtracting(1...2)
    expectEqual(sub.byteCount, 2)
    expectEqual(sub.unsafeLoad(as: UInt8.self), 1)
  }
}

suite.test("_mutatingExtracting() bounds checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Byte offset range out of bounds", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  var span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._mutatingExtracting(2 ..< .max)
}

suite.test("_consumingExtracting()")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let c = 16
  let b = UnsafeMutableRawBufferPointer.allocate(byteCount: c, alignment: c)
  defer { b.deallocate() }
  _ = b.initializeMemory(as: UInt8.self, from: 0..<UInt8(c))

  var span = b.mutableBytes
  span = span._consumingExtracting(0..<2)
  expectEqual(span.byteCount, 2)
  expectEqual(span.unsafeLoad(as: UInt8.self), 0)

  span = b.mutableBytes
  span = span._consumingExtracting(..<2)
  expectEqual(span.byteCount, 2)
  expectEqual(span.unsafeLoad(as: UInt8.self), 0)

  span = b.mutableBytes
  span = span._consumingExtracting(...)
  expectEqual(span.byteCount, c)
  expectEqual(span.unsafeLoad(as: UInt8.self), 0)

  span = b.mutableBytes
  span = span._consumingExtracting(2...)
  expectEqual(span.byteCount, c-2)
  expectEqual(span.unsafeLoad(as: UInt8.self), 2)

  span = b.mutableBytes._consumingExtracting(1...2)
  expectEqual(span.byteCount, 2)
  expectEqual(span.unsafeLoad(as: UInt8.self), 1)
}

suite.test("_consumingExtracting() bounds checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Byte offset range out of bounds", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  let span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._consumingExtracting(2 ..< .max)
}

suite.test("_mutatingExtracting(unchecked:)")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 32
  var b = (0..<capacity).map(UInt8.init)
  b.withUnsafeMutableBytes {
    var span = MutableRawSpan(_unsafeBytes: $0.prefix(8))
    let beyond = span._mutatingExtracting(unchecked: 16...23)
    expectEqual(beyond.byteCount, 8)
    let fromBeyond = beyond.unsafeLoad(as: UInt8.self)
    expectEqual(fromBeyond, 16)
  }
}

suite.test("_consumingExtracting(unchecked:)")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 32
  var b = (0..<capacity).map(UInt8.init)
  b.withUnsafeMutableBytes {
    let span = MutableRawSpan(_unsafeBytes: $0.prefix(8))
    let beyond = span._consumingExtracting(unchecked: 16...23)
    expectEqual(beyond.byteCount, 8)
    let fromBeyond = beyond.unsafeLoad(as: UInt8.self)
    expectEqual(fromBeyond, 16)
  }
}

suite.test("_mutatingExtracting prefixes")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBytes {
    var prefix: MutableRawSpan
    var span = MutableRawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity)

    prefix = span._mutatingExtracting(first: 1)
    expectEqual(prefix.unsafeLoad(as: UInt8.self), 0)

    prefix = span._mutatingExtracting(first: capacity)
    expectEqual(
      prefix.unsafeLoad(fromByteOffset: capacity-1, as: UInt8.self),
      UInt8(capacity-1)
    )

    prefix = span._mutatingExtracting(first: capacity + 5)
    expectEqual(prefix.byteCount, capacity)

    prefix = span._mutatingExtracting(droppingLast: capacity)
    expectEqual(prefix.isEmpty, true)

    prefix = span._mutatingExtracting(droppingLast: 1)
    expectEqual(
      prefix.unsafeLoad(fromByteOffset: capacity-2, as: UInt8.self),
      UInt8(capacity-2)
    )

    prefix = span._mutatingExtracting(droppingLast: capacity + 5)
    expectEqual(prefix.byteCount, 0)
  }

  do {
    let b = UnsafeMutableRawBufferPointer(start: nil, count: 0)
    var span = MutableRawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span._mutatingExtracting(first: 1).byteCount, b.count)
    expectEqual(span._mutatingExtracting(droppingLast: 1).byteCount, b.count)
  }
}

suite.test("_mutatingExtracting(first:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't have a prefix of negative length", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  var span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._mutatingExtracting(first: -1)
}

suite.test("_mutatingExtracting(droppingLast:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't drop a negative number of bytes", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  var span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._mutatingExtracting(droppingLast: -1)
}

suite.test("_consumingExtracting prefixes")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBytes {
    var span = $0.mutableBytes
    expectEqual(span.byteCount, capacity)

    span = span._consumingExtracting(first: 1)
    expectEqual(span.unsafeLoad(as: UInt8.self), 0)

    span = $0.mutableBytes._consumingExtracting(first: capacity)
    expectEqual(
      span.unsafeLoad(fromByteOffset: capacity-1, as: UInt8.self),
      UInt8(capacity-1)
    )

    span = $0.mutableBytes._consumingExtracting(first: capacity + 5)
    expectEqual(span.byteCount, capacity)

    span = $0.mutableBytes._consumingExtracting(droppingLast: capacity)
    expectEqual(span.isEmpty, true)

    span = $0.mutableBytes._consumingExtracting(droppingLast: 1)
    expectEqual(
      span.unsafeLoad(fromByteOffset: capacity-2, as: UInt8.self),
      UInt8(capacity-2)
    )

    span = $0.mutableBytes._consumingExtracting(droppingLast: capacity + 5)
    expectEqual(span.byteCount, 0)
  }

  do {
    let b = UnsafeMutableRawBufferPointer(start: nil, count: 0)
    var span = MutableRawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    span = b.mutableBytes._consumingExtracting(first: 1)
    expectEqual(span.byteCount, b.count)
    span = b.mutableBytes._consumingExtracting(droppingLast: 1)
    expectEqual(span.byteCount, b.count)
  }
}

suite.test("_consumingExtracting(first:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't have a prefix of negative length", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  let span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._consumingExtracting(first: -1)
}

suite.test("_consumingExtracting(droppingLast:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't drop a negative number of bytes", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  let span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._consumingExtracting(droppingLast: -1)
}

suite.test("_mutatingExtracting suffixes")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBytes {
    var prefix: MutableRawSpan
    var span = MutableRawSpan(_unsafeBytes: $0)
    expectEqual(span.byteCount, capacity)

    prefix = span._mutatingExtracting(last: capacity)
    expectEqual(prefix.unsafeLoad(as: UInt8.self), 0)

    prefix = span._mutatingExtracting(last: capacity-1)
    expectEqual(prefix.unsafeLoad(as: UInt8.self), 1)

    prefix = span._mutatingExtracting(last: 1)
    expectEqual(prefix.unsafeLoad(as: UInt8.self), UInt8(capacity-1))

    prefix = span._mutatingExtracting(last: capacity + 5)
    expectEqual(prefix.byteCount, capacity)

    prefix = span._mutatingExtracting(droppingFirst: capacity)
    expectTrue(prefix.isEmpty)

    prefix = span._mutatingExtracting(droppingFirst: 1)
    expectEqual(prefix.unsafeLoad(as: UInt8.self), 1)

    prefix = span._mutatingExtracting(droppingFirst: capacity + 5)
    expectEqual(prefix.byteCount, 0)
  }

  do {
    let b = UnsafeMutableRawBufferPointer(start: nil, count: 0)
    var span = MutableRawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    expectEqual(span._mutatingExtracting(last: 1).byteCount, b.count)
    expectEqual(span._mutatingExtracting(droppingFirst: 1).byteCount, b.count)
  }
}

suite.test("_mutatingExtracting(last:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't have a suffix of negative length", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  var span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._mutatingExtracting(last: -1)
}

suite.test("_mutatingExtracting(droppingFirst:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't drop a negative number of bytes", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  var span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._mutatingExtracting(droppingFirst: -1)
}

suite.test("_consumingExtracting suffixes")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBytes {
    var span = $0.mutableBytes
    expectEqual(span.byteCount, capacity)

    span = span._consumingExtracting(last: capacity)
    expectEqual(span.unsafeLoad(as: UInt8.self), 0)

    span = $0.mutableBytes._consumingExtracting(last: capacity-1)
    expectEqual(span.unsafeLoad(as: UInt8.self), 1)

    span = $0.mutableBytes._consumingExtracting(last: 1)
    expectEqual(span.unsafeLoad(as: UInt8.self), UInt8(capacity-1))

    span = $0.mutableBytes._consumingExtracting(last: capacity + 5)
    expectEqual(span.byteCount, capacity)

    span = $0.mutableBytes._consumingExtracting(droppingFirst: capacity)
    expectEqual(span.isEmpty, true)

    span = $0.mutableBytes._consumingExtracting(droppingFirst: 1)
    expectEqual(span.unsafeLoad(as: UInt8.self), 1)

    span = $0.mutableBytes._consumingExtracting(droppingFirst: capacity + 5)
    expectEqual(span.byteCount, 0)
  }

  do {
    let b = UnsafeMutableRawBufferPointer(start: nil, count: 0)
    var span = MutableRawSpan(_unsafeBytes: b)
    expectEqual(span.byteCount, b.count)
    span = b.mutableBytes._consumingExtracting(last: 1)
    expectEqual(span.byteCount, b.count)
    span = b.mutableBytes._consumingExtracting(droppingFirst: 1)
    expectEqual(span.byteCount, b.count)
  }
}

suite.test("_consumingExtracting(last:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't have a suffix of negative length", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  let span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._consumingExtracting(last: -1)
}

suite.test("_consumingExtracting(droppingFirst:) bound checking")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.crashOutputMatches("Can't drop a negative number of bytes", when: _isDebugAssertConfiguration())
.code {
  var b: ContiguousArray<Int> = [1, 2, 3, 4]
  let span = MutableRawSpan(elements: b.mutableSpan)
  expectCrashLater()
  _ = span._consumingExtracting(droppingFirst: -1)
}

suite.test("MutableRawSpan from UnsafeMutableRawBufferPointer")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: capacity*MemoryLayout<Int64>.stride,
    alignment: MemoryLayout<Int64>.alignment
  )
  defer {
    b.deallocate()
  }
  _ = b.initializeMemory(as: Int64.self, fromContentsOf: 0..<Int64(capacity))

  var span = b.mutableBytes
  span.storeBytes(of: 3, toByteOffset: 10, as: UInt16.self)

  _ = consume span

  let v = b.load(fromByteOffset: 8, as: Int64.self)
  expectNotEqual(v, 1)
}

suite.test("init(mutating:)")
.require(.stdlib_6_4).code {

  var array = ContiguousArray(0..<4)
  var mutableSpan = array.mutableSpan
  var bytes = MutableRawSpan(mutating: &mutableSpan)
  expectEqual(bytes.byteCount, 4 * MemoryLayout<Int>.stride)
  let v = bytes.load(fromByteOffset: MemoryLayout<Int>.stride, as: Int.self)
  expectEqual(v, 1)
  bytes.storeBytes(of: Int(42), toByteOffset: 0, as: Int.self)
  expectEqual(array[0], 42)
}

suite.test("init(elements:)")
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt32>([0xaa, 0xbb, 0xcc])
  let bytes = MutableRawSpan(elements: array.mutableSpan)
  expectEqual(bytes.byteCount, 3 * MemoryLayout<UInt32>.stride)
  let v = bytes.load(fromByteOffset: MemoryLayout<UInt32>.stride, as: UInt32.self)
  expectEqual(v, 0xbb)
}

suite.test("MutableRawSpan subscript")
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt8>([10, 20, 30, 40])
  var mutableSpan = array.mutableSpan
  var bytes = mutableSpan.mutableBytes
  expectEqual(bytes[0], 10)
  expectEqual(bytes[3], 40)
  bytes[1] = 99
  expectEqual(bytes[1], 99)
  expectEqual(array[1], 99)
}

suite.test("MutableRawSpan subscript bounds underflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt8>([10, 20, 30, 40])
  var mutableSpan = array.mutableSpan
  let bytes = mutableSpan.mutableBytes
  expectCrashLater()
  _ = bytes[-1]
}

suite.test("MutableRawSpan subscript bounds overflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt8>([10, 20, 30, 40])
  var mutableSpan = array.mutableSpan
  let bytes = mutableSpan.mutableBytes
  expectCrashLater()
  _ = bytes[4]
}

suite.test("MutableRawSpan unchecked subscript")
.require(.stdlib_6_4).code {

  var array = ContiguousArray<UInt8>([10, 20, 30, 40])
  var mutableSpan = array.mutableSpan
  var bytes = mutableSpan.mutableBytes._consumingExtracting(first: 3)
  expectEqual(unsafe bytes[unchecked: 0], 10)
  expectEqual(unsafe bytes[unchecked: 3], 40)
  unsafe bytes[unchecked: 0] = 77
  expectEqual(bytes[0], 77)
}

suite.test("MutableRawSpan safe loading with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var array = ContiguousArray<UInt8>(repeating: 0, count: 8)
  var mutableSpan = array.mutableSpan
  var bytes = mutableSpan.mutableBytes
  bytes[0] = 0x01
  bytes[1] = 0x02

  let big = bytes.load(fromByteOffset: 0, as: UInt16.self, .bigEndian)
  expectEqual(big, 0x0102)

  let little = bytes.load(fromByteOffset: 0, as: UInt16.self, .littleEndian)
  expectEqual(little, 0x0201)

  let native = bytes.load(fromByteOffset: 0, as: UInt16.self, .native)
  let plain = bytes.load(fromByteOffset: 0, as: UInt16.self)
  expectEqual(native, plain)
}

suite.test("MutableRawSpan storeBytes with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var array = ContiguousArray<UInt8>(repeating: 0, count: 8)
  var mutableSpan = array.mutableSpan
  var bytes = mutableSpan.mutableBytes

  bytes.storeBytes(of: 0x0102, toByteOffset: 0, as: UInt16.self, .bigEndian)
  expectEqual(bytes[0], 0x01)
  expectEqual(bytes[1], 0x02)

  bytes.storeBytes(of: 0x0304, toByteOffset: 2, as: UInt16.self, .littleEndian)
  expectEqual(bytes[2], 0x04)
  expectEqual(bytes[3], 0x03)

  bytes.storeBytes(of: 0x0506, toByteOffset: 4, as: UInt16.self, .native)
  let native = bytes.load(fromByteOffset: 4, as: UInt16.self)
  expectEqual(native, 0x0506)
}

suite.test("MutableRawSpan storeBytes repeating with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var array = ContiguousArray<UInt8>(repeating: 0, count: 8)
  var mutableSpan = array.mutableSpan
  var bytes = mutableSpan.mutableBytes

  bytes.storeBytes(
    repeating: UInt16(0x0102), count: 4, as: UInt16.self, .bigEndian
  )
  for i in 0..<4 {
    let offset = i * MemoryLayout<UInt16>.stride
    expectEqual(bytes[offset], 0x01)
    expectEqual(bytes[offset + 1], 0x02)
  }
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

suite.test("MutableRawSpan Sendability")
.require(.stdlib_6_2).code {
  let buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: 1, alignment: 2)
  defer { buffer.deallocate() }

  let span = MutableRawSpan(_unsafeBytes: buffer)
  send(span)
}
