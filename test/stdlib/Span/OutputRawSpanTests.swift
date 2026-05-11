//===--- OutputRawSpanTests.swift -----------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-strict-memory-safety -enable-experimental-feature Lifetimes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

var suite = TestSuite("OutputRawSpan Tests")
defer { runAllTests() }

@safe
private struct Allocation: ~Copyable {
  let allocation: UnsafeMutableRawBufferPointer
  var byteCount: Int? = nil

  init(byteCount: Int = 1) {
    precondition(byteCount >= 0)
    unsafe allocation = .allocate(byteCount: byteCount, alignment: 16)
  }

  var isEmpty: Bool { (byteCount ?? 0) == 0 }

  mutating func initialize<E>(
    _ body: (inout OutputRawSpan) throws(E) -> Void
  ) throws(E) {
    if byteCount != nil { fatalError() }
    var outputBuffer: OutputRawSpan
    outputBuffer = unsafe OutputRawSpan(buffer: allocation, initializedCount: 0)
    do {
      try body(&outputBuffer)
      let initialized = unsafe outputBuffer.finalize(for: allocation)
      byteCount = initialized
    }
    catch {
      outputBuffer.removeAll()
      let initialized = unsafe outputBuffer.finalize(for: allocation)
      assert(initialized == 0)
      throw error
    }
  }

  borrowing func withSpan<E, R: ~Copyable>(
    _ body: (borrowing RawSpan) throws(E) -> R
  ) throws(E) -> R {
    unsafe try body(RawSpan(_unsafeBytes: allocation[0..<(byteCount ?? 0)]))
  }

  var bytes: RawSpan {
    @_lifetime(borrow self)
    get {
      let span = unsafe RawSpan(_unsafeBytes: allocation[..<(byteCount ?? 0)])
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  deinit {
    unsafe allocation.deallocate()
  }
}

enum MyTestError: Error { case error }

private struct Padded: BitwiseCopyable {
  var storage: (Int64, Int8)
}

suite.test("Create OutputRawSpan")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutableRawBufferPointer.allocate(byteCount: c, alignment: 16)
  defer { unsafe allocation.deallocate() }

  let ob = unsafe OutputRawSpan(buffer: allocation, initializedCount: 0)
  let initialized = unsafe ob.finalize(for: allocation)
  expectEqual(initialized, 0)
}

suite.test("deinit without relinquishing memory")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutableRawBufferPointer.allocate(byteCount: c, alignment: 16)
  defer { unsafe allocation.deallocate() }

  var ob = unsafe OutputRawSpan(buffer: allocation, initializedCount: 0)
  // OutputRawSpan(buffer: Slice(base: allocation, bounds: 0..<c))
  ob.append(repeating: 65, count: 12, as: UInt8.self)
  expectEqual(ob.byteCount, 12)
  _ = ob
}

suite.test("basic properties")
.require(.stdlib_6_2).code {

  let c = 12
  var array = ContiguousArray(repeating: UInt8.zero, count: c)
  array.withUnsafeMutableBytes {
    let initialized = c/4
    var ob = unsafe OutputRawSpan(buffer: $0, initializedCount: initialized)

    expectEqual(ob.capacity, c)
    expectEqual(ob.freeCapacity, c-initialized)
    expectFalse(ob.isEmpty)
    expectFalse(ob.isFull)
    expectEqual(ob.byteOffsets, 0..<initialized)

    while !ob.isFull { ob.append(1) }
    expectEqual(ob.freeCapacity, 0)
    expectFalse(ob.isEmpty)
    expectTrue(ob.isFull)
    expectEqual(ob.byteOffsets, 0..<c)

    while !ob.isEmpty { ob.removeLast() }
    expectEqual(ob.freeCapacity, ob.capacity)
    expectTrue(ob.isEmpty)
    expectFalse(ob.isFull)
    expectEqual(ob.byteOffsets, 0..<0)
  }
}

suite.test("mutableBytes property")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    for i in 0..<UInt8(4) {
      $0.append(i)
    }
    expectEqual($0.byteCount, 4)
    expectEqual($0[0], 0)
    do {
      var mb = $0.mutableBytes
      expectEqual(mb.byteCount, 4)
      mb.storeBytes(of: UInt8(99), toByteOffset: 0, as: UInt8.self)
    }
    expectEqual($0[0], 99)
  }
}

suite.test("append single elements")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 48)
  let c = 10
  let d = c + MemoryLayout<Float64>.size
  a.initialize {
    for i in 0...c {
      $0.append(UInt8(i))
    }
    let oops = $0.removeLast()
    expectEqual(Int(oops), c)
    $0.append(Float64(c), as: Float64.self)
    expectEqual($0.byteCount, d)
  }
  expectNotNil(a.byteCount)
  expectEqual(a.byteCount, d)
  a.withSpan {
    expectEqual($0.byteCount, d)
    let span = $0.extracting(first: c)
    expectEqual(span.byteCount, c)
    for o in span.byteOffsets {
      let byte = unsafe span.unsafeLoad(fromByteOffset: o, as: UInt8.self)
      expectEqual(Int(byte), o)
    }
  }
}

suite.test("initialize buffer with repeated elements")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 48)
  let c = UInt8(10)
  a.initialize {
    $0.append(repeating: c, count: Int(c), as: UInt8.self)
    let oops = $0.removeLast()
    expectEqual(oops, c)
    expectEqual($0.byteCount, Int(c-1))
  }
  a.withSpan {
    expectEqual($0.byteCount, Int(c-1))
    for o in $0.byteOffsets {
      let byte = unsafe $0.unsafeLoad(fromByteOffset: o, as: UInt8.self)
      expectEqual(byte, c)
    }
  }
}

suite.test("deinitialize buffer")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 48)
  do {
    try a.initialize {
      $0.append(0)
      $0.append(1)
      expectTrue($0.byteCount > 0)
      throw MyTestError.error
    }
  }
  catch MyTestError.error {
    expectEqual(a.isEmpty, true)
  }
  catch {
    expectUnreachable("Unexpected error thrown")
  }
}

suite.test("OutputRawSpan.withUnsafeMutableBytes")
.require(.stdlib_6_2).code {

  let c = 49
  var a = Allocation(byteCount: c)
  a.initialize {
    unsafe $0.withUnsafeMutableBytes {
      expectEqual($0.count, c)
      for i in $0.indices {
        unsafe $0.storeBytes(of: UInt8(i), toByteOffset: i, as: UInt8.self)
        $1 += 1
      }
    }

    expectEqual($0.byteCount, c)
    for i in $0.byteOffsets {
      let loaded = unsafe $0.bytes.unsafeLoad(fromByteOffset: i, as: UInt8.self)
      expectEqual(loaded , UInt8(i))
    }
  }
}

suite.test("removeLast(k:)")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    for i in 0..<UInt8(8) {
      $0.append(i)
    }
    expectEqual($0.byteCount, 8)
    $0.removeLast(3)
    expectEqual($0.byteCount, 5)
    $0.removeLast(5)
    expectTrue($0.isEmpty)
  }
  expectEqual(a.byteCount, 0)
}

suite.test("append one unsafe")
.require(.stdlib_6_2).code {

  var a = Allocation(byteCount: 64)
  let p = Padded(storage: (42, 7))
  a.initialize {
    unsafe $0.append(p, as: Padded.self)
    expectEqual($0.byteCount, MemoryLayout<Padded>.size)

    let v = unsafe $0.bytes.unsafeLoad(as: Padded.self)
    expectEqual(v.storage.0, 42)
    expectEqual(v.storage.1, 7)
  }
}

suite.test("append one safe")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 64)
  a.initialize {
    $0.append(200_000, as: UInt32.self)
    $0.append(400_000_000_000, as: UInt64.self)
    let total = MemoryLayout<UInt32>.size + MemoryLayout<UInt64>.size
    expectEqual($0.byteCount, total)

    let u32 = $0.bytes.load(fromByteOffset: 0, as: UInt32.self)
    expectEqual(u32, 200_000)
    let u64 = $0.bytes.load(fromByteOffset: 4, as: UInt64.self)
    expectEqual(u64, UInt64(4e11))
  }
}

suite.test("append one overflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 1)
  a.initialize {
    $0.append(1)
    expectCrashLater()
    $0.append(2)
  }
}

suite.test("append with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(UInt16(0x0102), as: UInt16.self, .bigEndian)
    expectEqual($0[0], 0x01)
    expectEqual($0[1], 0x02)

    $0.append(UInt16(0x0304), as: UInt16.self, .littleEndian)
    expectEqual($0[2], 0x04)
    expectEqual($0[3], 0x03)
  }
}

suite.test("append repeating unsafe")
.require(.stdlib_6_2).code {

  let count = 3
  var a = Allocation(byteCount: count * MemoryLayout<Padded>.stride)
  let p = Padded(storage: (99, -1))
  a.initialize {
    unsafe $0.append(repeating: p, count: count, as: Padded.self)

    for i in 0..<count {
      let v = unsafe $0.bytes.unsafeLoad(
        fromByteOffset: i * MemoryLayout<Padded>.stride, as: Padded.self
      )
      expectEqual(v.storage.0, 99)
      expectEqual(v.storage.1, -1)
    }
  }
}

suite.test("append repeating safe")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 64)
  a.initialize {
    $0.append(repeating: 0xabcd, count: 4, as: UInt16.self)
    expectEqual($0.byteCount, 4 * MemoryLayout<UInt16>.stride)

    for i in 0..<4 {
      let loaded = $0.bytes.load(
        fromByteOffset: i * MemoryLayout<UInt16>.stride, as: UInt16.self
      )
      expectEqual(loaded, 0xabcd)
    }
  }
}

suite.test("append repeating negative count")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    expectCrashLater()
    $0.append(repeating: 0, count: -1, as: UInt8.self)
  }
}

suite.test("append repeating with ByteOrder")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { expectTrue(false); return }

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(
      repeating: 0x0102, count: 4, as: UInt16.self, .bigEndian
    )
    for i in 0..<4 {
      let offset = i * MemoryLayout<UInt16>.stride
      expectEqual($0[offset],     0x01)
      expectEqual($0[offset + 1], 0x02)
    }

    $0.removeAll()
    $0.append(
      repeating: 0xabcd, count: 4, as: UInt16.self, .littleEndian
    )
    for i in 0..<4 {
      let offset = i * MemoryLayout<UInt16>.stride
      expectEqual($0[offset],     0xcd)
      expectEqual($0[offset + 1], 0xab)
    }

  }
}

suite.test("append(upTo:as:initializingWith:)")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 64)
  a.initialize {
    $0.append(upTo: 3, as: UInt32.self) { typedSpan in
      typedSpan.append(0xaaaaaaaa)
      typedSpan.append(0xbbbbbbbb)
      typedSpan.append(0xcccccccc)
    }
    expectEqual($0.byteCount, 3 * MemoryLayout<UInt32>.stride)
    for i in $0.byteOffsets {
      switch i / MemoryLayout<UInt32>.stride {
      case 0: expectEqual($0[i], 0xaa)
      case 1: expectEqual($0[i], 0xbb)
      case 2: expectEqual($0[i], 0xcc)
      default: expectUnreachable("Unexpected value")
      }
    }
  }
}

suite.test("append(upTo:as:initializingWith:) partial fill")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 64)
  try! a.initialize {
    do throws(MyTestError) {
      try $0.append(upTo: 4, as: UInt32.self) {
        typedSpan throws(MyTestError) in
        typedSpan.append(0x11111111)
        typedSpan.append(0x22222222)
        throw MyTestError.error
      }
    }
    catch MyTestError.error {}
  }

  let bytes = a.bytes
  expectEqual(bytes.byteCount, 2 * MemoryLayout<UInt32>.stride)
  let v0 = bytes.load(fromByteOffset: 0, as: UInt32.self)
  let v1 = bytes.load(
    fromByteOffset: MemoryLayout<UInt32>.stride, as: UInt32.self
  )
  expectEqual(v0, 0x11111111)
  expectEqual(v1, 0x22222222)
}

suite.test("append(upTo:as:initializingWith:) overflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    expectCrashLater()
    $0.append(upTo: 100, as: UInt32.self) {
      _ in
    }
  }
}

suite.test("append(upTo:as:initializingWith:) negative count")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    expectCrashLater()
    $0.append(upTo: -1, as: UInt32.self) { _ in }
  }
}

suite.test("append(upTo:as:initializingWith:) misaligned")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(UInt8(1))
    expectEqual($0.byteCount, 1)
    expectCrashLater()
    $0.append(upTo: 1, as: UInt32.self) { _ in }
  }
}

suite.test("subscript getter")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    for i in 0..<UInt8(4) {
      $0.append(i &* 10)
    }
    expectEqual($0[0], 0)
    expectEqual($0[1], 10)
    expectEqual($0[2], 20)
    expectEqual($0[3], 30)
  }
}

suite.test("subscript setter")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(0)
    $0.append(0)
    expectEqual($0[0], 0)
    expectEqual($0[1], 0)
    $0[0] = 0xaa
    $0[1] = 0xbb
    expectEqual($0[0], 0xaa)
    expectEqual($0[1], 0xbb)
  }
}

suite.test("subscript bounds underflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(UInt8(1))
    expectCrashLater()
    _ = $0[-1]
  }
}

suite.test("subscript bounds overflow")
.require(.crashTesting)
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    $0.append(UInt8(1))
    expectCrashLater()
    _ = $0[1]
  }
}

suite.test("unchecked subscript getter")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    for i in 0..<UInt8(4) {
      $0.append(i &* 10)
    }
    $0.removeAll()
    expectEqual(unsafe $0[unchecked: 0], 0)
    expectEqual(unsafe $0[unchecked: 3], 30)
  }
}

suite.test("unchecked subscript setter")
.require(.stdlib_6_4).code {

  var a = Allocation(byteCount: 16)
  a.initialize {
    unsafe $0[unchecked: 0] = 0xcc
    unsafe $0[unchecked: 1] = 0xdd
    expectEqual(unsafe $0[unchecked: 0], 0xcc)
    expectEqual(unsafe $0[unchecked: 1], 0xdd)
  }
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

suite.test("OutputRawSpan Sendability")
.require(.stdlib_6_2).code {
  send(OutputRawSpan())
}
