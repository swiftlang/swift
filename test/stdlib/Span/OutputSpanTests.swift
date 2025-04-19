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
    var outputBuffer = OutputSpan<T>(buffer: allocation, initializedCount: 0)
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

enum MyTestError: Error { case error }

suite.test("Create Output Buffer")
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

suite.test("initialize buffer from Sequence")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = Allocation(of: 48, Int.self)
  a.initialize {
    $0.append(fromContentsOf: 0..<18)
  }
  a.withSpan {
    expectEqual($0.count, 18)
    for i in $0.indices {
      expectEqual($0[i], i)
    }
  }
}

suite.test("initialize buffer from noncontiguous Collection")
.require(.stdlib_6_2).code {
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
.require(.stdlib_6_2).code {
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
.require(.stdlib_6_2).code {
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
.require(.stdlib_6_2).code {
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
.require(.stdlib_6_2).code {
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
}

suite.test("mutate with MutableSpan prefix")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: 10)
  defer { b.deallocate() }

  var span = unsafe OutputSpan(buffer: b, initializedCount: 0)
  expectEqual(span.count, 0)
  span.append(fromContentsOf: 1...9)
  expectEqual(span.count, 9)

  var mutable = span.mutableSpan
//    span.append(20) // exclusivity violation
  for i in 0..<mutable.count {
    mutable[i] *= 2
  }

  span.append(20)

  let initialized = span.finalize(for: b)
  expectEqual(initialized, 10)
  expectTrue(b.elementsEqual((0..<10).map({2*(1+$0)})))
  b.deinitialize()
}

suite.test("InlineArray initialization")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a: [Int] = []

  let i = InlineArray<10, Int> {
    (o: inout OutputSpan<Int>) in
    for _ in 0..<o.capacity {
      let r = Int.random(in: 10...99)
      a.append(r)
      o.append(r)
    }
  }
  expectEqual(a.count, i.count)
  for j in i.indices {
    expectEqual(a[j], i[j])
  }
}

suite.test("InlineArray initialization underflow")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  expectCrashLater()
  _ = InlineArray<4, Int> {
    $0.append(1)
  }
}

#if false
suite.test("InlineArray initialization throws 0")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  enum LocalError: Error { case error }

  class I {
    static public var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  let a: InlineArray<4, I>
  do throws(LocalError) {
    var c = 1
    a = try InlineArray(first: I()) {
      i throws(LocalError) in
      c += 1
      if c < 4 {
        return I()
      }
      print(I.count)
      throw LocalError.error
    }
    _ = a
  } catch {
    print(error, I.count)
    expectEqual(I.count, 0)
  }
}

suite.test("InlineArray initialization throws")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  enum LocalError: Error { case error }

  class I {
    static var count = 0
    init() { Self.count += 1 }
    deinit { Self.count -= 1 }
  }

  let a: InlineArray<4, I>
  do throws(LocalError) {
    a = try InlineArray {
      o throws(LocalError) in
      o.append(I())
      o.append(I())
      o.append(I())
      o.append(I())
      expectEqual(I.count, 4)
      print(I.count)
      throw LocalError.error
    }
    _ = a
  } catch {
    print(error, I.count)
    expectEqual(I.count, 0)
  }
}
#endif

suite.test("Array initialization")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let requested = 32
  let actual = requested/2

  let array: [UInt8] = Array(capacity: requested, initializingWith: { output in
    for i in 0..<actual {
      output.append(UInt8(clamping: UInt8(i)))
    }
  })
  expectEqual(array.count, actual)
  expectEqual(array.elementsEqual(0..<UInt8(actual)), true)
  expectGE(array.capacity, requested)
}

suite.test("ContiguousArray initialization")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let requested = 32
  let actual = requested/2

  let array = ContiguousArray<UInt8>(capacity: requested) { output in
    for i in 0..<actual {
      output.append(UInt8(clamping: UInt8(i)))
    }
  }
  expectEqual(array.count, actual)
  expectEqual(array.elementsEqual(0..<UInt8(actual)), true)
  expectGE(array.capacity, requested)
}

suite.test("ContiguousArray initialization throws")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  do {
    let array = try ContiguousArray<Int>(capacity: 8) { _ in
      throw MyTestError.error
    }
    expectTrue(false)
    _ = array
  } catch MyTestError.error {
  } catch {
    expectTrue(false)
  }
}

suite.test("String initialization")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let requested = 32
  let actual = requested/2

  let c = UInt8(ascii: "A")
  let string = String(capacity: requested, initializingUTF8With: {
    for i in 0..<actual {
      $0.append(c + UInt8(clamping: i))
    }
  })
  expectEqual(string.utf8.count, 16)
  expectTrue(string.utf8.elementsEqual(c..<(c+16)))
}
