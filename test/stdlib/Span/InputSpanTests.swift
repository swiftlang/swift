//===--- InputSpanTests.swift ---------------------------------------------===//
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
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest

var suite = TestSuite("InputSpan Tests")
defer { runAllTests() }

// Initialize the last `count` storage slots of `buffer` with
// `LifetimeTracked` instances (numbered `0..<count`).
func initializeSuffix(
  of buffer: UnsafeMutableBufferPointer<LifetimeTracked>, count: Int
) {
  let offset = buffer.count - count
  for i in 0..<count {
    buffer.initializeElement(at: offset + i, to: LifetimeTracked(i))
  }
}

suite.test("Create InputSpan")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var span = InputSpan<Int>()
  expectEqual(span.capacity, 0)
  expectEqual(span.count, 0)
  expectTrue(span.isEmpty)
  expectTrue(span.isFull)
  expectEqual(span.span.count, 0)

  span = InputSpan(buffer: .init(start: nil, count: 0), initializedCount: 0)
  expectEqual(span.capacity, 0)
  expectEqual(span.count, 0)
  expectTrue(span.isEmpty)
  expectTrue(span.isFull)
  expectEqual(span.span.count, 0)

  let c = 48
  let allocation = UnsafeMutableBufferPointer<Int>.allocate(capacity: c)
  defer { allocation.deallocate() }

  span = unsafe InputSpan(buffer: allocation, initializedCount: 0)
  expectEqual(span.capacity, c)
  expectEqual(span.freeCapacity, c)
  expectTrue(span.isEmpty)
  expectFalse(span.isFull)
  var remaining = span.finalize(for: allocation)
  expectEqual(remaining, 0)

  // exercise the initializer from a slice
  let slice = allocation[2..<8]
  _ = slice.initialize(fromContentsOf: 0..<6)

  span = unsafe InputSpan(buffer: slice, initializedCount: 6)
  expectEqual(span.capacity, 6)
  expectTrue(span.isFull)
  expectEqual(span.removeFirst(), 0)
  remaining = span.finalize(for: slice)
  expectEqual(remaining, 5)
  expectTrue(allocation[3..<8].elementsEqual(1..<6))
  allocation[3..<8].deinitialize()
}

suite.test("init with invalid initializedCount")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  expectCrashLater(withDebugMessage: "InputSpan count is not within capacity")
  _ = unsafe InputSpan(buffer: b, initializedCount: -1)
}

suite.test("init with invalid buffer")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  // cause an internal initializer to return an invalid UMBP
  let invalid = UnsafeMutableBufferPointer<Int>(_uncheckedStart: nil, count: 1)

  expectCrashLater()
  _ = unsafe InputSpan(buffer: invalid, initializedCount: 0)
}

suite.test("indices and subscript")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 8
  let initialized = 5
  let b = UnsafeMutableBufferPointer<SIMD3<UInt8>>.allocate(capacity: capacity)
  defer { b.deallocate() }
  _ = b[(capacity - initialized)...].initialize(
    fromContentsOf: (0..<initialized).map { SIMD3(repeating: UInt8($0)) }
  )

  var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
  expectEqual(span.count, initialized)
  expectEqual(span.freeCapacity, capacity - initialized)
  expectTrue(span.indices.elementsEqual(0..<initialized))
  for i in span.indices {
    expectEqual(span[i], SIMD3(repeating: UInt8(i)))
  }

  span[0] &+= 10
  expectEqual(span[0], SIMD3(repeating: 10))
  expectEqual(b[capacity - initialized], SIMD3(repeating: 10))

  span.swapAt(0, initialized - 1)
  expectEqual(span[0], SIMD3(repeating: UInt8(initialized - 1)))
  expectEqual(span[initialized - 1], SIMD3(repeating: 10))

  span.removeAll()
  expectEqual(span.finalize(for: b), 0)
}

suite.test("subscript out of bounds")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  let span = unsafe InputSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "index out of bounds")
  _ = span[1]
}

suite.test("swapAt index out of bounds")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }
  b.initializeElement(at: 3, to: 0)

  var span = unsafe InputSpan(buffer: b, initializedCount: 1)
  expectCrashLater()
  span.swapAt(0, 1)
}

suite.test("prepend and removeFirst")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  func triple(_ i: Int) -> (UInt8, UInt8, UInt8) {
    (UInt8(i), UInt8(i + 100), UInt8(i + 200))
  }

  let c = 10
  let b = UnsafeMutableBufferPointer<(UInt8,UInt8,UInt8)>.allocate(capacity: c)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  for i in (0..<c).reversed() {
    span.prepend(triple(i))
  }
  expectTrue(span.isFull)
  expectTrue(b.elementsEqual((0..<c).map(triple), by: { $0 == $1 }))

  expectEqual(span.removeFirst(), triple(0))
  expectEqual(span.popFirst()!, triple(1))
  expectEqual(span.count, c - 2)
  span.removeFirst(3)
  expectEqual(span.count, c - 5)
  expectEqual(span[0], triple(5))
  span.removeFirst(0)
  expectEqual(span.count, c - 5)

  span.prepend(repeating: triple(42), count: 0)
  expectEqual(span.count, c - 5)
  expectEqual(span[0], triple(5))

  span.prepend(repeating: triple(42), count: 3)
  expectEqual(span.count, c - 2)
  expectTrue(span.span.withUnsafeBufferPointer {
    $0.elementsEqual(
      [42, 42, 42, 5, 6, 7, 8, 9].map(triple), by: { $0 == $1 }
    )
  })

  while let _ = span.popFirst() {}
  expectTrue(span.isEmpty)
  expectNil(span.popFirst())
  expectEqual(span.finalize(for: b), 0)
}

suite.test("prepend overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 1)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  span.prepend(0)
  expectCrashLater(withDebugMessage: "InputSpan capacity overflow")
  span.prepend(1)
}

suite.test("prepend repeating negative count")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "Can't prepend a negative number of values")
  span.prepend(repeating: 0, count: -1)
}

suite.test("prepend repeating overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  span.prepend(0)
  expectCrashLater(withDebugMessage: "InputSpan capacity overflow")
  span.prepend(repeating: 1, count: 4)
}

suite.test("removeFirst too many items")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }
  b.initializeElement(at: 3, to: 0)

  var span = unsafe InputSpan(buffer: b, initializedCount: 1)
  expectCrashLater(withDebugMessage: "InputSpan underflow")
  span.removeFirst(2)
}

suite.test("removeFirst from empty span")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "InputSpan underflow")
  _ = span.removeFirst()
}

suite.test("removeFirst negative quantity")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }
  b.initializeElement(at: 3, to: 0)

  var span = unsafe InputSpan(buffer: b, initializedCount: 1)
  expectCrashLater(
    withDebugMessage: "Can't remove a negative number of elements"
  )
  span.removeFirst(-1)
}

suite.test("removeAll on partially-initialized buffer")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 8
  let initialized = 3
  let b = UnsafeMutableBufferPointer<LifetimeTracked>.allocate(
    capacity: capacity
  )
  defer { b.deallocate() }
  initializeSuffix(of: b, count: initialized)
  expectEqual(LifetimeTracked.instances, initialized)

  var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
  span.removeAll()
  expectEqual(LifetimeTracked.instances, 0)
  expectTrue(span.isEmpty)
  span.removeAll()
  expectEqual(span.finalize(for: b), 0)
}

suite.test("deinit destroys remaining elements")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 8
  let initialized = 5
  let b = UnsafeMutableBufferPointer<LifetimeTracked>.allocate(
    capacity: capacity
  )
  defer { b.deallocate() }
  initializeSuffix(of: b, count: initialized)

  do {
    var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
    expectEqual(span.removeFirst().value, 0)
    expectEqual(LifetimeTracked.instances, initialized - 1)
    _ = consume span
  }
  expectEqual(LifetimeTracked.instances, 0)
}

suite.test("span and mutableSpan")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 6
  let initialized = 4
  let b = UnsafeMutableBufferPointer<SIMD3<UInt8>>.allocate(capacity: capacity)
  defer { b.deallocate() }
  _ = b[(capacity - initialized)...].initialize(
    fromContentsOf: (0..<initialized).map { SIMD3(repeating: UInt8($0)) }
  )

  var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
  do {
    let s = span.span
    expectEqual(s.count, initialized)
    for i in s.indices {
      expectEqual(s[i], SIMD3(repeating: UInt8(i)))
    }
  }
  do {
    var m = span.mutableSpan
    expectEqual(m.count, initialized)
    for i in m.indices {
      m[i] &*= 2
    }
  }
  for i in span.indices {
    expectEqual(span[i], SIMD3(repeating: UInt8(2 * i)))
  }
  span.removeAll()
  _ = span.finalize(for: b)
}

suite.test("withUnsafeMutableBufferPointer")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let c = 10
  let b = UnsafeMutableBufferPointer<[Int]>.allocate(capacity: c)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  unsafe span.withUnsafeMutableBufferPointer { buffer, count in
    expectEqual(buffer.count, c)
    expectEqual(count, 0)
    for i in buffer.indices.reversed() {
      buffer.initializeElement(at: i, to: Array(repeating: i, count: i))
      count += 1
    }
  }
  expectEqual(span.count, c)
  for i in span.indices {
    expectEqual(span[i], Array(repeating: i, count: i))
  }

  // Consume the first half through the buffer pointer.
  unsafe span.withUnsafeMutableBufferPointer { buffer, count in
    let start = buffer.count - count
    let half = count / 2
    buffer.extracting(start..<(start + half)).deinitialize()
    count -= half
  }
  expectEqual(span.count, c - c / 2)
  expectEqual(span[0], Array(repeating: c / 2, count: c / 2))

  span.removeAll()
  expectEqual(span.finalize(for: b), 0)

  var empty = InputSpan<Int>()
  unsafe empty.withUnsafeMutableBufferPointer { buffer, count in
    expectEqual(buffer.count, 0)
    expectEqual(count, 0)
  }
}

private struct TestError: Error, Equatable {}

suite.test("withUnsafeMutableBufferPointer return value and throwing")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 6
  let b = UnsafeMutableBufferPointer<LifetimeTracked>.allocate(
    capacity: capacity
  )
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)

  // The closure's result is passed through, and the count is updated correctly.
  let result = unsafe span.withUnsafeMutableBufferPointer {
    initializeSuffix(of: $0, count: 2)
    $1 = 2
    return "initialized \($1)"
  }
  expectEqual(result, "initialized 2")
  expectEqual(span.count, 2)
  expectEqual(LifetimeTracked.instances, 2)

  // The thrown error error propagates, and the count is updated correctly.
  do throws(TestError) {
    try unsafe span.withUnsafeMutableBufferPointer {
      (buffer, count) throws(TestError) -> Void in
      let end = buffer.count - count
      buffer.initializeElement(at: end - 1, to: LifetimeTracked(-1))
      count += 1
      throw TestError()
    }
    expectUnreachable("withUnsafeMutableBufferPointer should have thrown")
  } catch {
    expectEqual(error, TestError())
  }
  expectEqual(span.count, 3)
  expectEqual(span[0].value, -1)
  expectEqual(LifetimeTracked.instances, 3)

  span.removeAll()
  expectEqual(LifetimeTracked.instances, 0)
  expectEqual(span.finalize(for: b), 0)
}

suite.test("withUnsafeMutableBufferPointer capacity overflow")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  var span = unsafe InputSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "InputSpan capacity overflow")
  unsafe span.withUnsafeMutableBufferPointer { buffer, count in
    count = buffer.count + 1
  }
}

suite.test("consumePrefix")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 10
  let initialized = 8
  let b = UnsafeMutableBufferPointer<LifetimeTracked>.allocate(
    capacity: capacity
  )
  defer { b.deallocate() }
  initializeSuffix(of: b, count: initialized)

  var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
  do {
    var prefix = span.consumePrefix(maxCount: 3)
    expectEqual(prefix.count, 3)
    expectTrue(prefix.isFull)
    expectEqual(prefix.removeFirst().value, 0)
    expectEqual(prefix[0].value, 1)
    // The remaining two elements are destroyed with `prefix`.
  }
  expectEqual(LifetimeTracked.instances, initialized - 3)
  expectEqual(span.count, initialized - 3)
  expectEqual(span[0].value, 3)

  do {
    let none = span.consumePrefix(maxCount: 0)
    expectEqual(none.count, 0)
  }
  expectEqual(span.count, initialized - 3)

  do {
    let rest = span.consumePrefix(maxCount: .max)
    expectEqual(rest.count, initialized - 3)
    expectEqual(rest[0].value, 3)
  }
  expectTrue(span.isEmpty)
  expectEqual(LifetimeTracked.instances, 0)

  do {
    let none = span.consumePrefix(maxCount: 1)
    expectEqual(none.count, 0)
  }
  expectEqual(span.finalize(for: b), 0)

  var empty = InputSpan<Int>()
  let none = empty.consumePrefix(maxCount: 4)
  expectEqual(none.count, 0)
}

suite.test("consumePrefix negative count")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  var span = InputSpan<Int>()
  expectCrashLater(withDebugMessage: "Can't consume a negative number")
  _ = span.consumePrefix(maxCount: -1)
}

suite.test("finalize identity mismatch")
.require(.crashTesting)
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
  defer { b.deallocate() }

  let span = unsafe InputSpan(buffer: b, initializedCount: 0)
  expectCrashLater(withDebugMessage: "InputSpan identity mismatch")
  _ = span.finalize(for: b.extracting(1...))
}

suite.test("Iterable conformance")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let capacity = 8
  let initialized = 5
  let b = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: capacity)
  defer { b.deallocate() }
  _ = b[(capacity - initialized)...].initialize(
    fromContentsOf: 0..<UInt8(initialized)
  )

  var span = unsafe InputSpan(buffer: b, initializedCount: initialized)
  expectEqual(span.underestimatedCount, initialized)
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
  expectEqual(collected, Array(0..<UInt8(initialized)))

  span.removeAll()
  _ = span.finalize(for: b)
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

private struct NCSendable: ~Copyable, Sendable {}

suite.test("InputSpan Sendability")
.require(.stdlib_6_5).code {
  guard #available(SwiftStdlib 6.5, *) else { return }

  let buffer = UnsafeMutableBufferPointer<NCSendable>.allocate(capacity: 1)
  defer { buffer.deallocate() }

  let span = unsafe InputSpan(buffer: buffer, initializedCount: 0)
  send(span)
  _ = span.finalize(for: buffer)
}
