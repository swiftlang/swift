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

@available(SwiftStdlib 6.2, *)
struct Allocation<T>: ~Copyable {
  let allocation: UnsafeMutablePointer<T>
  let capacity: Int
  var count: Int? = nil

  init(of count: Int = 1, _ t: T.Type) {
    precondition(count >= 0)
    capacity = count
    allocation = UnsafeMutablePointer<T>.allocate(capacity: capacity)
  }

  var isEmpty: Bool { (count ?? 0) == 0 }

  mutating func initialize<E>(
    _ body: (/* mutating */ inout OutputSpan<T>) throws(E) -> Void
  ) throws(E) {
    if count != nil { fatalError() }
    var outputBuffer = OutputSpan<T>(
      _initializing: .init(start: allocation, count: capacity)
    )
    do {
      try body(&outputBuffer)
      let initialized = outputBuffer.relinquishBorrowedMemory()
      assert(initialized.baseAddress == allocation)
      count = initialized.count
    }
    catch {
      outputBuffer.removeAll()
      let empty = outputBuffer.relinquishBorrowedMemory()
      assert(empty.baseAddress == allocation)
      assert(empty.count == 0)
      throw error
    }
  }

  borrowing func withSpan<E, R: ~Copyable>(
    _ body: (borrowing Span<T>) throws(E) -> R
  ) throws(E) -> R {
    try body(Span(_unsafeStart: allocation, count: count ?? 0))
  }

  deinit {
    if let count {
      allocation.deinitialize(count: count)
    }
    allocation.deallocate()
  }
}

enum MyTestError: Error { case error }

suite.test("Create Output Buffer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutablePointer<UInt8>.allocate(capacity: c)
  defer { allocation.deallocate() }

  let ob = OutputSpan(_initializing: .init(start: allocation, count: c))
  // OutputSpan(_initializing: allocation, capacity: c)
  let initialized = ob.relinquishBorrowedMemory()
  expectNotNil(initialized.baseAddress)
  expectEqual(initialized.count, 0)
}

suite.test("deinit without relinquishing memory")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: c)
  defer { allocation.deallocate() }

  var ob = OutputSpan(_initializing: allocation)
  // OutputSpan(_initializing: Slice(base: allocation, bounds: 0..<c))
  ob.append(repeating: 65, count: 12)
  expectEqual(ob.count, 12)
  _ = ob
}

suite.test("append single elements")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
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
  a.withSpan {
    expectEqual($0.count, c)
    for i in $0.indices {
      expectEqual($0[i], i)
    }
  }
}

suite.test("initialize buffer of BitwiseCopyable elements")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let c = 48
  let allocation: UnsafeMutableRawBufferPointer
  allocation = .allocate(byteCount: c, alignment: 8)
  defer { allocation.deallocate() }

  let rb = allocation.bindMemory(to: Int16.self)
  var ob = OutputSpan(_initializing: .init(rebasing: rb[4...]))
  //OutputSpan<Int16>(_initializing: allocation[8...])
  let r = Int16.max>>2
  ob.append(r)
  _ = ob.relinquishBorrowedBytes()

  let o = allocation.load(fromByteOffset: 8, as: Int16.self)
  expectEqual(o, r)
}

suite.test("initialize buffer with repeated elements")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
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

suite.test("initialize buffer from Sequence")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  a.initialize {
    var it = $0.append(from: 0..<18)
    expectNil(it.next())
  }
  a.withSpan {
    expectEqual($0.count, 18)
    for i in $0.indices {
      expectEqual($0[i], i)
    }
  }
}

suite.test("initialize buffer from noncontiguous Collection")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  let c = 24
  a.initialize {
    $0.append(fromContentsOf: 0..<c)

    let prefix = $0.span
    expectEqual(prefix.count, c)
    for i in prefix.indices {
      expectEqual(prefix[i], i)
    }
  }
}

suite.test("initialize buffer from contiguous Collection")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  let c = 24
  a.initialize {
    $0.append(fromContentsOf: Array(0..<c))

    let prefix = $0.span
    expectEqual(prefix.count, c)
    for i in prefix.indices {
      expectEqual(prefix[i], i)
    }
  }
}

suite.test("initialize buffer from Span")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  let c = 24
  a.initialize {
    os in
    var array = Array(0..<c)
    array.withUnsafeMutableBufferPointer {
      let span = MutableSpan(_unsafeElements: $0)
      os.append(fromContentsOf: span)
    }
  }
  a.withSpan {
    expectEqual($0.count, c)
    for i in $0.indices {
      expectEqual($0[i], i)
    }
  }
}

suite.test("initialize buffer from empty contiguous Collection")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  a.initialize {
    $0.append(fromContentsOf: [])
  }
  a.withSpan { span in
    expectEqual(span.count, 0)
  }
  expectTrue(a.isEmpty)
}

suite.test("moveAppend()")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  class I {
    let i: Int
    init(_ i: Int) {
      self.i = i
    }
  }
  let c = 20
  let b = UnsafeMutableBufferPointer<I>.allocate(capacity: c)
  for i in 0..<c {
    b.initializeElement(at: i, to: I(i))
  }
  var a = Allocation(of: 48, I.self)
  a.initialize {
    $0.moveAppend(fromContentsOf: b)
    $0.moveAppend(fromContentsOf: b[c..<c])
  }
  expectFalse(a.isEmpty)
  a.withSpan {
    expectEqual($0.count, c)
  }
}

suite.test("deinitialize buffer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
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
}

suite.test("mutate with MutableSpan prefix")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 10)
  defer { b.deallocate() }

  var span = OutputSpan(_initializing: b)
  expectEqual(span.count, 0)
  span.append(fromContentsOf: 1...9)
  expectEqual(span.count, 9)

  var mutable = span.mutableSpan
//    span.append(20) // exclusivity violation
  for i in 0..<mutable.count {
    mutable[i] *= 2
  }

  span.append(20)

  let r = span.relinquishBorrowedMemory()
  expectTrue(r.elementsEqual((0..<10).map({2*(1+$0)})))
  r.deinitialize()
}
