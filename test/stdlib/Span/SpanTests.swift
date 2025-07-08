//===--- SpanTests.swift --------------------------------------------------===//
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

@available(SwiftStdlib 6.2, *)
extension Span where Element: Equatable {

  /// Returns a Boolean value indicating whether this and another span
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A span to compare to this one.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: Self) -> Bool {
    guard count == other.count else { return false }
    if count == 0 { return true }

    //FIXME: This could be short-cut
    //       with a layout constraint where stride equals size,
    //       as long as there is at most 1 unused bit pattern.
    // if Element is BitwiseEquatable {
    // return _swift_stdlib_memcmp(lhs.baseAddress, rhs.baseAddress, count) == 0
    // }
    for o in 0..<count {
      if self[unchecked: o] != other[unchecked: o] { return false }
    }
    return true
  }

  /// Returns a Boolean value indicating whether this span and a Collection
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Collection to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Collection<Element>) -> Bool {
    let equal = other.withContiguousStorageIfAvailable {
      _elementsEqual(Span(_unsafeElements: $0))
    }
    if let equal { return equal }

    guard count == other.count else { return false }
    if count == 0 { return true }

    return _elementsEqual(AnySequence(other))
  }

  /// Returns a Boolean value indicating whether this span and a Sequence
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Sequence to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Sequence<Element>) -> Bool {
    var offset = 0
    for otherElement in other {
      if offset >= count { return false }
      if self[unchecked: offset] != otherElement { return false }
      offset += 1
    }
    return offset == count
  }
}

suite.test("Initialize with ordinary element")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var s = (0..<capacity).map({ "\(#file)+\(#function)--\($0)" })
  s.withUnsafeBufferPointer {
    var span = Span(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    let pointer = $0.baseAddress!
    span = Span(_unsafeStart: pointer, count: $0.count)
    expectEqual(span.count, capacity)
  }

  s.withUnsafeMutableBufferPointer {
    var span = Span(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    let pointer: UnsafeMutablePointer = $0.baseAddress!
    span = Span(_unsafeStart: pointer, count: $0.count)
    expectEqual(span.count, capacity)

    span = Span()
    expectEqual(span.count, 0)
  }
}

suite.test("Initialize with BitwiseCopyable element")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    var span = Span(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    let pointer = $0.baseAddress!
    span = Span(_unsafeStart: pointer, count: $0.count)
    expectEqual(span.count, capacity)
  }

  a.withUnsafeMutableBufferPointer {
    var span = Span(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    let pointer: UnsafeMutablePointer = $0.baseAddress!
    span = Span(_unsafeStart: pointer, count: $0.count)
    expectEqual(span.count, capacity)
  }

  a.withUnsafeBytes {
    let b = Span<UInt>(_unsafeBytes: $0)
    expectEqual(b.count, capacity)

    let r = Span<Int8>(_unsafeBytes: $0)
    expectEqual(r.count, capacity*MemoryLayout<Int>.stride)

    let p = $0.baseAddress!
    let span = Span<Int>(
      _unsafeStart: p, byteCount: capacity*MemoryLayout<Int>.stride
    )
    expectEqual(span.count, capacity)
  }

  a.withUnsafeMutableBytes {
    let b = Span<UInt>(_unsafeBytes: $0)
    expectEqual(b.count, capacity)

    let p = $0.baseAddress!
    let span = Span<Int>(
      _unsafeStart: p, byteCount: capacity*MemoryLayout<Int>.stride
    )
    expectEqual(span.count, capacity)
  }
}

suite.test("isEmpty")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    expectFalse(span.isEmpty)

    let emptyBuffer = UnsafeBufferPointer(rebasing: $0.dropFirst(capacity))
    let empty = Span(_unsafeElements: emptyBuffer)
    expectTrue(empty.isEmpty)
  }
}

suite.test("RawSpan from Span")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let count = 4
  let array = Array(0..<count)
  array.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    let raw  = RawSpan(_elements: span)
    expectEqual(raw.byteCount, span.count*MemoryLayout<Int>.stride)
  }
}

suite.test("indices property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    expectEqual(span.count, span.indices.count)

    var i = 0
    for j in span.indices {
      expectEqual(i, j)
      i += 1
    }
  }
}

suite.test("_elementsEqual(_: Span)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array<Int>(unsafeUninitializedCapacity: capacity) {
    for i in $0.indices {
      $0.initializeElement(at: i, to: i)
    }
    $1 = $0.count
  }
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)

    expectEqual(span._elementsEqual(span._extracting(first: 1)), false)
    expectEqual(span._extracting(0..<0)._elementsEqual(span._extracting(last: 0)), true)
    expectEqual(span._elementsEqual(span), true)
    expectEqual(span._extracting(0..<3)._elementsEqual(span._extracting(last: 3)), false)

    let copy = span.withUnsafeBufferPointer(Array.init)
    copy.withUnsafeBufferPointer {
      let spanOfCopy = Span(_unsafeElements: $0)
      expectTrue(span._elementsEqual(spanOfCopy))
    }
  }
}

suite.test("_elementsEqual(_: some Collection)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)

    expectEqual(span._elementsEqual(a), true)
    expectEqual(span._extracting(0..<0)._elementsEqual([]), true)
    expectEqual(span._elementsEqual(a.dropLast()), false)
  }
}

suite.test("_elementsEqual(_: some Sequence")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)

    let s = AnySequence(a)
    expectEqual(span._elementsEqual(s), true)
    expectEqual(span._extracting(0..<(capacity-1))._elementsEqual(s), false)
    expectEqual(span._elementsEqual(s.dropFirst()), false)
  }
}

suite.test("subscript with ordinary element")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = (0..<capacity).map(String.init)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    expectEqual(span[3], String(3))
  }
}

suite.test("subscript with BitwiseCopyable element")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    expectEqual(span[3], 3)
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
  let b = (0..<capacity).map(String.init)
  b.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    let sub1 = span._extracting(0..<2)
    let sub2 = span._extracting(..<2)
    let sub3 = span._extracting(...)
    let sub4 = span._extracting(2...)
    expectTrue(sub1._elementsEqual(sub2))
    expectTrue(sub3._elementsEqual(span))
    expectFalse(sub4._elementsEqual(sub3))
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
  let b = (0..<capacity).map(String.init)
  b.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    let prefix = span._extracting(0..<8)
    let beyond = prefix._extracting(unchecked: 16...23)
    expectEqual(beyond.count, 8)
    expectEqual(beyond[0], "16")
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
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span(_unsafeElements: $0)
    expectEqual(span.count, capacity)
    expectEqual(span._extracting(first: 1)[0], 0)
    expectEqual(span._extracting(first: capacity)[capacity-1], capacity-1)
    expectEqual(span._extracting(droppingLast: capacity).count, 0)
    expectEqual(span._extracting(droppingLast: 1)[capacity-2], capacity-2)
  }

  do {
    let b = UnsafeBufferPointer<Int>(start: nil, count: 0)
    let span = Span(_unsafeElements: b)
    expectEqual(span.count, b.count)
    expectEqual(span._extracting(first: 1).count, b.count)
    expectEqual(span._extracting(droppingLast: 1).count, b.count)
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
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    let span = Span<Int>(_unsafeElements: $0)
    expectEqual(span.count, capacity)
    expectEqual(span._extracting(last: capacity)[0], 0)
    expectEqual(span._extracting(last: capacity-1)[0], 1)
    expectEqual(span._extracting(last: 1)[0], capacity-1)
    expectEqual(span._extracting(droppingFirst: capacity).count, 0)
    expectEqual(span._extracting(droppingFirst: 1)[0], 1)
  }

  do {
    let b = UnsafeBufferPointer<Int>(start: nil, count: 0)
    let span = Span(_unsafeElements: b)
    expectEqual(span.count, b.count)
    expectEqual(span._extracting(last: 1).count, b.count)
    expectEqual(span._extracting(droppingFirst: 1).count, b.count)
  }
}

suite.test("withUnsafeBufferPointer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 64
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    ub in
    let span = Span(_unsafeElements: ub)
    span.withUnsafeBufferPointer {
      let i = Int.random(in: 0..<$0.count)
      expectEqual($0[i], ub[i])
    }

    let emptyBuffer = UnsafeBufferPointer(rebasing: ub[0..<0])
    expectEqual(emptyBuffer.baseAddress, ub.baseAddress)

    let emptySpan = Span(_unsafeElements: emptyBuffer)
    emptySpan.withUnsafeBufferPointer {
      expectNil($0.baseAddress)
    }
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
  let a = Array(0..<capacity)
  a.withUnsafeBufferPointer {
    ub in
    let span = Span(_unsafeElements: ub)

    span.withUnsafeBytes {
      let i = Int.random(in: $0.indices)
      expectEqual($0.load(fromByteOffset: i, as: UInt8.self), ub[i])
    }

    let emptyBuffer = UnsafeBufferPointer(rebasing: ub[0..<0])
    expectEqual(emptyBuffer.baseAddress, ub.baseAddress)

    let emptySpan = Span(_unsafeElements: emptyBuffer)
    emptySpan.withUnsafeBytes {
      expectNil($0.baseAddress)
    }
  }
}

suite.test("isIdentical(to:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 8)
  _ = b.initialize(fromContentsOf: 0..<8)
  defer { b.deallocate() }

  let span = Span(_unsafeElements: b)
  let pre = span._extracting(first: 6)
  let suf = span._extracting(last: 6)

  expectFalse(
    pre.isIdentical(to: suf)
  )
  expectFalse(
    pre.isIdentical(to: span)
  )
  expectTrue(
    pre._extracting(last: 4).isIdentical(to: suf._extracting(first: 4))
  )
}

suite.test("indices(of:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 8)
  _ = b.initialize(fromContentsOf: 0..<8)
  defer { b.deallocate() }

  let span = Span(_unsafeElements: b)
  let subSpan1 = span._extracting(first: 6)
  let subSpan2 = span._extracting(last: 6)
  let emptySpan = span._extracting(first: 0)
/*  This isn't relevant until we can support unaligned spans
  let unalignedSpan = RawSpan(_unsafeSpan: span)
                        ._extracting(droppingFirst: 6)
                        ._extracting(droppingLast: 2)
                        .unsafeView(as: Int.self)
*/
  let nilBuffer = UnsafeBufferPointer<Int>(start: nil, count: 0)
  let nilSpan = Span(_unsafeElements: nilBuffer)

  var bounds: Range<Int>?
  bounds = span.indices(of: subSpan1)
  expectEqual(bounds, span.indices.prefix(6))
  bounds = span.indices(of: subSpan2)
  expectEqual(bounds, span.indices.suffix(6))
  bounds = subSpan2.indices(of: subSpan1)
  expectNil(bounds)
  bounds = subSpan1.indices(of: subSpan2)
  expectNil(bounds)
  bounds = subSpan2.indices(of: span)
  expectNil(bounds)
  bounds = nilSpan.indices(of: emptySpan)
  expectNil(bounds)
  bounds = span.indices(of: nilSpan)
  expectNil(bounds)
  bounds = nilSpan.indices(of: nilSpan)
  expectEqual(bounds, 0..<0)
}

suite.test("initialize from raw memory")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableRawBufferPointer.allocate(byteCount: 64, alignment: 8)
  defer { b.deallocate() }
  let initialized = b.initializeMemory(as: UInt8.self, fromContentsOf: 0..<64)
  expectEqual(initialized.count, 64)
  defer { initialized.deinitialize() }

  func test<T>(_ span: Span<T>) -> T {
    span[0]
  }

  // let span = Span<Int32>(_unsafeBytes: b.dropFirst().dropLast(7))

  let suffix = b.dropFirst(4)
  let span = Span<Int32>(_unsafeBytes: suffix)
  let first = test(span)
  expectEqual(first, 0x07060504)
}

private func send(_: some Sendable & ~Escapable) {}

private struct NCSendable: ~Copyable, Sendable {}

suite.test("Span Sendability")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let buffer = UnsafeMutableBufferPointer<NCSendable>.allocate(capacity: 1)
  defer { buffer.deallocate() }
  buffer.initializeElement(at: 0, to: NCSendable())
  defer { buffer.deinitialize() }

  let span = Span(_unsafeElements: buffer)
  send(span)
}
