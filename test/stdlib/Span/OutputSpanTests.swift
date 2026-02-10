//===--- OutputSpanTests.swift --------------------------------------------===//
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

var suite = TestSuite("OutputSpan Tests")
defer { runAllTests() }

struct Allocation<T>: ~Copyable {
  let allocation: UnsafeMutableBufferPointer<T>
  var count: Int? = nil

  init(of count: Int = 1, _ t: T.Type) {
    precondition(count >= 0)
    allocation = .allocate(capacity: count)
  }

  var isEmpty: Bool { (count ?? 0) == 0 }

  mutating func initialize<E>(
    _ body: (inout OutputSpan<T>) throws(E) -> Void
  ) throws(E) {
    if count != nil { fatalError() }
    var allocation = allocation
    if allocation.count == 0 { allocation = .init(start: nil, count: 0) }
    var outputBuffer = OutputSpan(buffer: allocation, initializedCount: 0)
    do {
      try body(&outputBuffer)
      let initialized = outputBuffer.finalize(for: allocation)
      count = initialized
    }
    catch {
      outputBuffer.removeAll()
      let initialized = outputBuffer.finalize(for: allocation)
      assert(initialized == 0)
      throw error
    }
  }

  borrowing func withSpan<E, R: ~Copyable>(
    _ body: (borrowing Span<T>) throws(E) -> R
  ) throws(E) -> R {
    try body(Span(_unsafeElements: allocation[0..<count!]))
  }

  deinit {
    if let count {
      allocation.prefix(count).deinitialize()
    }
    allocation.deallocate()
  }
}

enum MyTestError: Error, Equatable { case error }

suite.test("Create OutputSpan")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: c)
  defer { allocation.deallocate() }

  let ob = unsafe OutputSpan(buffer: allocation, initializedCount: 0)
  let initialized = ob.finalize(for: allocation)
  expectEqual(initialized, 0)
}

suite.test("deinit without relinquishing memory")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: c)
  defer { allocation.deallocate() }

  var ob = unsafe OutputSpan(buffer: allocation, initializedCount: 0)
  // OutputSpan(buffer: Slice(base: allocation, bounds: 0..<c))
  ob.append(repeating: 65, count: 12)
  expectEqual(ob.count, 12)
  _ = ob
}

suite.test("append single elements")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  let c = 10
  a.initialize {
    for i in 0...c {
      $0.append(i)
    }
    let oops = $0.removeLast()
    expectEqual(oops, c)
  }
  expectNotNil(a.count)
  expectEqual(a.count, c)
  a.withSpan {
    expectEqual($0.count, c)
    for i in $0.indices {
      expectEqual($0[i], i)
    }
  }
}

suite.test("initialize buffer with repeated elements")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  let c = 10
  a.initialize {
    $0.append(repeating: c, count: c)
    let oops = $0.removeLast()
    expectEqual(oops, c)
    expectEqual($0.count, c-1)
  }
  a.withSpan {
    expectEqual($0.count, c-1)
    for i in $0.indices {
      expectEqual($0[i], c)
    }
  }
}

suite.test("indices property")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: capacity)
  defer { b.deallocate() }
  _ = b.initialize(fromContentsOf: 0..<capacity)
  defer { b.deinitialize() }

  let span = unsafe OutputSpan(buffer: b, initializedCount: capacity)
  expectEqual(span.indices.count, capacity)
  let equal = span.indices.elementsEqual(0..<capacity)
  expectTrue(equal)
}

suite.test("IndexingSubscript")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: capacity)
  defer { b.deallocate() }
  _ = b.initialize(fromContentsOf: 0..<capacity)
  defer { b.deinitialize() }

  var span = unsafe OutputSpan(buffer: b, initializedCount: capacity)
  expectEqual(span[0], b.first)

  span[0] += 1
  expectEqual(span[0], b.first)
}

suite.test("deinitialize buffer")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  do {
    try a.initialize {
      $0.append(0)
      $0.append(1)
      expectTrue($0.count > 0)
      throw MyTestError.error
    }
  }
  catch MyTestError.error {
    expectEqual(a.isEmpty, true)
  }
  catch {
    expectTrue(false)
  }

  a = Allocation(of: 0, Int.self)
  do {
    try a.initialize {
      expectEqual($0.freeCapacity, 0)
      throw MyTestError.error
    }
  }
  catch MyTestError.error {
    expectEqual(a.isEmpty, true)
  }
  catch {
    expectTrue(false)
  }
}

suite.test("InlineArray initialization")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let i = InlineArray<10, Int> {
    (o: inout OutputSpan<Int>) in
    expectEqual(o.count, 0)
    for i in 0..<o.capacity {
      o.append(i)
    }
    expectEqual(o.freeCapacity, 0)
  }
  for j in i.indices {
    expectEqual(j, i[j])
  }
}

suite.test("InlineArray initialization underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  expectCrashLater()
  _ = InlineArray<4, Int> {
    $0.append(1)
  }
  expectUnreachable("InlineArray initializer should have trapped.")
}

suite.test("InlineArray initialization throws")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  class I {
    static var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  let a: InlineArray<4, I>
  do throws(MyTestError) {
    a = try InlineArray {
      o throws(MyTestError) in
      o.append(I())
      o.append(I())
      o.append(I())
      expectEqual(I.count, 3)
      throw MyTestError.error
    }
    _ = a
  } catch {
    expectEqual(I.count, 0)
  }
}

suite.test("OutputSpan.withUnsafeMutableBufferPointer")
.require(.stdlib_6_2).code {
  let c = 10
  var a = Allocation(of: c, [Int].self)
  a.initialize {
    $0.withUnsafeMutableBufferPointer {
      expectEqual($0.count, c)
      for i in $0.indices {
        $0.initializeElement(at: i, to: .init(repeating: i, count: i))
        $1 += 1
      }
    }
  }
  a.withSpan {
    expectEqual($0.count, c)
    for i in $0.indices {
      expectEqual($0[i], Array(repeating: i, count: i))
    }
  }
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

private struct NCSendable: ~Copyable, Sendable {}

suite.test("OutputSpan Sendability")
.require(.stdlib_6_2).code {
  let buffer = UnsafeMutableBufferPointer<NCSendable>.allocate(capacity: 1)
  defer { buffer.deallocate() }

  let span = OutputSpan(buffer: buffer, initializedCount: 0)
  send(span)
}

suite.test("Array initialization")
.require(.stdlib_6_2).code {

  var a: Array<Int>

  a = Array(capacity: 8) {
    span in
    span.append(0)
  }
  expectEqual(a.count, 1)
  expectGE(a.capacity, 8)

  a = Array(capacity: 8) {
    span in
    for i in 0..<8 {
      span.append(i)
    }
  }
  expectEqual(a.count, 8)
  expectGE(a.capacity, 8)
  expectTrue(a.elementsEqual(0..<8))
}

suite.test("Array initialization throws")
.require(.stdlib_6_2).code {
  class I {
    static var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  var a: [I] = []
  do throws(MyTestError) {
    a = try Array(capacity: 4) {
      o throws(MyTestError) in
      o.append(I())
      expectEqual(I.count, 1)
      throw MyTestError.error
    }
    expectUnreachable("Array initializer should have thrown.")
  } catch {
    expectEqual(I.count, 0)
    expectEqual(a.count, 0)
  }

  a = []
  do throws(MyTestError) {
    a = try Array(capacity: 0) {
      o throws(MyTestError) in
      expectEqual(I.count, 0)
      throw MyTestError.error
    }
    expectUnreachable("Array initializer should have thrown.")
  } catch {
    expectEqual(I.count, 0)
    expectEqual(a.count, 0)
  }
}

suite.test("Array initialization overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  _ = Array<Int>(capacity: 1) {
    span in
    span.append(1)
    span.append(2)
  }
  expectUnreachable("Array initializer should have trapped.")
}

suite.test("Array initialization underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  _ = Array<Int>(capacity: -1) {
    $0.removeAll()
  }
  expectUnreachable("Array initializer should have trapped.")
}

suite.test("Array append")
.require(.stdlib_6_2).code {

  let base = [0, 1, 2, 3]
  var a = base

  a.append(addingCapacity: 8) {
    span in
    span.append(0)
  }
  expectEqual(a.count, base.count+1)
  expectGE(a.capacity, base.count+8)

  a = base
  a.append(addingCapacity: 8) {
    span in
    while !span.isFull {
      span.append(span.count+base.count)
    }
  }
  expectEqual(a.count, base.count+8)
  expectGE(a.capacity, base.count+8)
  expectTrue(a.elementsEqual(0..<a.count))
}

suite.test("Array append throws")
.require(.stdlib_6_2).code {
  class I {
    static var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  var a = [I(), I(), I(), I()]
  expectEqual(a.count, 4)
  expectEqual(I.count, 4)
  do throws(MyTestError) {
    try a.append(addingCapacity: 8) {
      span throws(MyTestError) in
      for i in 1..<100 {
        span.append(I())
        expectEqual(I.count, 4+i)
        if I.count >= 6 { throw MyTestError.error }
      }
    }
    expectUnreachable("Array initializer should have thrown.")
  } catch {
    expectEqual(error, MyTestError.error)
  }

  // changes are preserved up to a thrown error
  expectEqual(a.count, 6)
  expectEqual(I.count, 6)
}

suite.test("Array append overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  var a: [Int] = []
  a.append(addingCapacity: 1) {
    span in
    span.append(1)
    span.append(2)
  }
  expectUnreachable("Array.append should have trapped.")
}

suite.test("Array append underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  var a: [Int] = []
  a.append(addingCapacity: -1) {
    $0.removeAll()
  }
  expectUnreachable("Array.append should have trapped.")
}

suite.test("ContiguousArray initialization")
.require(.stdlib_6_2).code {

  var a: ContiguousArray<Int>

  a = ContiguousArray(capacity: 8) {
    span in
    span.append(0)
  }
  expectEqual(a.count, 1)
  expectGE(a.capacity, 8)

  a = ContiguousArray(capacity: 8) {
    span in
    for i in 0..<8 {
      span.append(i)
    }
  }
  expectEqual(a.count, 8)
  expectGE(a.capacity, 8)
  expectTrue(a.elementsEqual(0..<8))

  a = []
  do throws(MyTestError) {
    a = try ContiguousArray(capacity: 8) {
      span throws(MyTestError) in
      span.append(0)
      throw MyTestError.error
    }
    expectUnreachable("ContiguousArray initializer should have thrown.")
  } catch {
    expectEqual(error, MyTestError.error)
    expectTrue(a.isEmpty)
  }

  a = []
  do throws(MyTestError) {
    a = try ContiguousArray(capacity: 0) {
      _ throws(MyTestError) in
      throw MyTestError.error
    }
    expectUnreachable("ContiguousArray initializer should have thrown.")
  } catch {
    expectEqual(error, MyTestError.error)
    expectTrue(a.isEmpty)
  }
}

suite.test("ContiguousArray initialization overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  _ = ContiguousArray<Int>(capacity: 1) {
    span in
    span.append(1)
    span.append(2)
  }
  expectUnreachable("ContiguousArray initializer should have trapped.")
}

suite.test("ContiguousArray initialization underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  _ = ContiguousArray<Int>(capacity: -1) {
    $0.removeAll()
  }
  expectUnreachable("ContiguousArray initializer should have trapped.")
}

suite.test("ContiguousArray append")
.require(.stdlib_6_2).code {

  let base = [0, 1, 2, 3]
  var a = base

  a.append(addingCapacity: 8) {
    span in
    span.append(0)
  }
  expectEqual(a.count, base.count+1)
  expectGE(a.capacity, base.count+8)

  a = base
  a.append(addingCapacity: 8) {
    span in
    while !span.isFull {
      span.append(span.count+base.count)
    }
  }
  expectEqual(a.count, base.count+8)
  expectGE(a.capacity, base.count+8)
  expectTrue(a.elementsEqual(0..<a.count))
}

suite.test("ContiguousArray append throws")
.require(.stdlib_6_2).code {
  class I {
    static var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  var a: ContiguousArray = [I(), I(), I(), I()]
  expectEqual(a.count, 4)
  expectEqual(I.count, 4)
  do throws(MyTestError) {
    try a.append(addingCapacity: 8) {
      span throws(MyTestError) in
      for i in 1..<100 {
        span.append(I())
        expectEqual(I.count, 4+i)
        if I.count >= 6 { throw MyTestError.error }
      }
    }
    expectUnreachable("Array initializer should have thrown.")
  } catch {
    expectEqual(error, MyTestError.error)
  }

  // changes are preserved up to a thrown error
  expectEqual(a.count, 6)
  expectEqual(I.count, 6)
}

suite.test("ContiguousArray append overflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  var a: [Int] = []
  a.append(addingCapacity: 1) {
    span in
    span.append(1)
    span.append(2)
  }
  expectUnreachable("ContiguousArray append should have trapped.")
}

suite.test("ContiguousArray.append underflow")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {

  expectCrashLater()
  var a: [Int] = [1, 2, 3]
  a.append(addingCapacity: -1) {
    $0.removeAll()
  }
  expectUnreachable("ContiguousArray.append should have trapped.")
}
