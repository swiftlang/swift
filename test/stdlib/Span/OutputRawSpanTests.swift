//===--- OutputRawSpanTests.swift -----------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-strict-memory-safety)

// REQUIRES: executable_test

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

  deinit {
    unsafe allocation.deallocate()
  }
}

enum MyTestError: Error { case error }

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
  unsafe ob.append(repeating: 65, count: 12, as: UInt8.self)
  expectEqual(ob.byteCount, 12)
  _ = ob
}

suite.test("append single elements")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(byteCount: 48)
  let c = 10
  let d = c + MemoryLayout<Float64>.size
  a.initialize {
    for i in 0...c {
      $0.append(UInt8(i))
    }
    let oops = $0.removeLast()
    expectEqual(Int(oops), c)
    unsafe $0.append(Float64(c), as: Float64.self)
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
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(byteCount: 48)
  let c = UInt8(10)
  a.initialize {
    unsafe $0.append(repeating: c, count: Int(c), as: UInt8.self)
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
  guard #available(SwiftStdlib 6.2, *) else { return }

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
    expectTrue(false)
  }
}

suite.test("OutputRawSpan.withUnsafeBytes")
.require(.stdlib_6_2).code {
  let c = 49
  var a = Allocation(byteCount: c)
  a.initialize {
    $0.withUnsafeMutableBytes {
      expectEqual($0.count, c)
      for i in $0.indices {
        $0.storeBytes(of: UInt8(i), toByteOffset: i, as: UInt8.self)
        $1 += 1
      }
    }
  }
  a.withSpan {
    expectEqual($0.byteCount, c)
    for i in $0.byteOffsets {
      expectEqual($0.unsafeLoad(fromByteOffset: i, as: UInt8.self), UInt8(i))
    }
  }
}

private func send(_: borrowing some Sendable & ~Copyable & ~Escapable) {}

suite.test("OutputRawSpan Sendability")
.require(.stdlib_6_2).code {
  let buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: 1, alignment: 2)
  defer { unsafe buffer.deallocate() }

  let span = unsafe OutputRawSpan(buffer: buffer, initializedCount: 0)
  send(span)
}
