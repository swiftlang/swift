//===--- MutableSpanTests.swift -------------------------------------------===//
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

var suite = TestSuite("MutableSpan Tests")
defer { runAllTests() }

suite.test("Initialize with ordinary element")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  let capacity = 4
  var s = (0..<capacity).map({ "\(#file)+\(#function)--\($0)" })
  s.withUnsafeMutableBufferPointer {
    var b = MutableSpan(_unsafeElements: $0)
    expectEqual(b.count, $0.count)

    b = MutableSpan()
    expectEqual(b.count, 0)
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
  a.withUnsafeMutableBufferPointer {
    let b = MutableSpan(_unsafeElements: $0)
    let c = b.count
    expectEqual(c, $0.count)
  }

  a.withUnsafeMutableBytes {
    let (rp, bc) = ($0.baseAddress!, $0.count)
    let b = MutableSpan<UInt>(_unsafeStart: rp, byteCount: bc)
    expectEqual(b.count, capacity)

    let stride = MemoryLayout<Int>.stride
    let r = MutableSpan<Int8>(_unsafeBytes: $0.dropFirst(stride))
    expectEqual(r.count, (capacity-1)*stride)
    expectEqual(r.count, bc-stride)
  }

  let v = UnsafeMutableRawBufferPointer(start: nil, count: 0)
  let m = MutableSpan<Int>(_unsafeBytes: v)
  expectEqual(m.count, 0)
}

suite.test("isEmpty")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  var array = [0, 1, 2]
  array.withUnsafeMutableBufferPointer {
    let span = MutableSpan(_unsafeElements: $0)
    let e = span.isEmpty
    expectFalse(e)
  }

  array = []
  array.withUnsafeMutableBufferPointer {
    let span = MutableSpan(_unsafeElements: $0)
    let e = span.isEmpty
    expectTrue(e)
  }
}

suite.test("Span from MutableSpan")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  var array = [0, 1, 2]
  array.withUnsafeMutableBufferPointer {
    let mutable = MutableSpan(_unsafeElements: $0)
    let immutable  = Span(_mutableSpan: mutable)
    expectEqual(mutable.count, immutable.count)
  }
}

suite.test("RawSpan from MutableSpan")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  let count = 4
  var array = Array(0..<count)
  array.withUnsafeMutableBufferPointer {
    let (p, c) = ($0.baseAddress!, $0.count)
    let span = MutableSpan(_unsafeStart: p, count: c)
    let bytes  = span.bytes
    expectEqual(bytes.byteCount, count*MemoryLayout<Int>.stride)
    let v = bytes.unsafeLoad(
      fromByteOffset: MemoryLayout<Int>.stride, as: Int.self
    )
    expectEqual(v, 1)
  }
}

suite.test("MutableRawSpan from MutableSpan")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let count = 4
  var array = Array(0..<count)
  expectEqual(array[0], 0)
  array.withUnsafeMutableBufferPointer {
    let (p, c) = ($0.baseAddress!, $0.count)
    var span = MutableSpan(_unsafeStart: p, count: c)
    var bytes  = span.mutableBytes
    expectEqual(bytes.byteCount, count*MemoryLayout<Int>.stride)
    bytes.storeBytes(of: 1, as: Int.self)
  }
  expectEqual(array[0], 1)
}

suite.test("indices property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeMutableBufferPointer {
    let view = MutableSpan(_unsafeElements: $0)
    expectEqual(view.count, view.indices.count)
    let equal = view.indices.elementsEqual(0..<view.count)
    expectTrue(equal)
  }
}

suite.test("IndexingSubscript")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeMutableBufferPointer {
    [first = a.first] in
    var v = MutableSpan(_unsafeElements: $0)
    expectEqual(v[0], first)

    v[0] += 1
    expectEqual(v[0], first?.advanced(by: 1))
  }

  var b = a.map(String.init)
  b.withUnsafeMutableBufferPointer {
    [first = b.first] in
    var v = MutableSpan(_unsafeElements: $0)
    expectEqual(v[0], first)

    v[0].append("!")
    expectEqual(v[0], first.map({$0+"!"}))
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
  var a = Array(0..<capacity)
  a.withUnsafeMutableBufferPointer {
    let view = MutableSpan(_unsafeElements: $0)
    view.withUnsafeBufferPointer { b in
      let i = Int(capacity/2)
      expectEqual(b[i], b[i])
    }
  }
}

suite.test("withUnsafeBytes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 64
  var a = Array(0..<capacity)
  let i = Int.random(in: a.indices)
  a.withUnsafeMutableBufferPointer {
    let view = MutableSpan(_unsafeElements: $0)
    view.withUnsafeBytes {
      expectEqual($0.load(fromByteOffset: i, as: UInt8.self), $0[i])
    }
  }
}

suite.test("withUnsafeMutableBufferPointer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 64
  var a = Array(0..<capacity)
  let i = Int.random(in: a.indices)
  a.withUnsafeMutableBufferPointer {
    var view = MutableSpan(_unsafeElements: $0)
    view.withUnsafeMutableBufferPointer {
      $0[i] += 1
    }

    let empty0 = UnsafeMutableBufferPointer(start: $0.baseAddress, count: 0)
    var emptySpan = MutableSpan(_unsafeElements: empty0)
    emptySpan.withUnsafeMutableBufferPointer {
      expectEqual($0.count, 0)
      expectNil($0.baseAddress)
    }
  }
  expectEqual(Int(a[i]), i+1)
}

suite.test("withUnsafeMutableBytes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity: UInt8 = 64
  var a = Array(0..<capacity)
  let i = Int.random(in: a.indices)
  a.withUnsafeMutableBufferPointer {
    var view = MutableSpan(_unsafeElements: $0)
    view.withUnsafeMutableBytes {
      $0.storeBytes(of: UInt8(i+1), toByteOffset: i, as: UInt8.self)
    }

    let empty0 = UnsafeMutableBufferPointer(start: $0.baseAddress, count: 0)
    var emptySpan = MutableSpan(_unsafeElements: empty0)
    emptySpan.withUnsafeMutableBytes {
      expectEqual($0.count, 0)
      expectNil($0.baseAddress)
    }
  }
  expectEqual(Int(a[i]), i+1)
}

private class ID {
  let id: Int
  init(id: Int) {
    self.id = id
  }
  deinit {
    // print(id)
  }
}

suite.test("update(repeating:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var a = (0..<8).map(ID.init(id:))
  expectEqual(a.map(\.id).contains(.max), false)
  a.withUnsafeMutableBufferPointer {
    var span = MutableSpan(_unsafeElements: $0)
    span.update(repeating: ID(id: .max))
  }
  expectEqual(a.allSatisfy({ $0.id == .max }), true)
}

suite.test("span property")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let count = 8
  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: count)
  _ = b.initialize(fromContentsOf: 0..<count)
  defer { b.deallocate() }
  let e = UnsafeBufferPointer<Int>(start: nil, count: 0)
  defer { _ = e }

  var m = MutableSpan<Int>(_unsafeElements: b)
  m[0] = 100
  expectEqual(m.count, count)
  expectEqual(m[0], 100)

  var s = m.span
  expectEqual(s.count, m.count)
  expectEqual(s[0], m[0])

  // we're done using `s` before it gets reassigned
  m.update(repeating: 7)

  s = m.span

//    m[0] = -1 // exclusivity violation

  expectEqual(s.count, m.count)
  expectEqual(s[0], m[0])
}

suite.test("swapAt")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let count = 8
  var array = Array(0..<count)
  array.withUnsafeMutableBufferPointer {
    var m = MutableSpan(_unsafeElements: $0)
    for i in 0..<(count/2) {
      m.swapAt(i, count - i - 1)
    }
  }

  expectEqual(array, (0..<count).reversed())
}

suite.test("_mutatingExtracting()")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var b = (0..<capacity).map(Int8.init)
  b.withUnsafeMutableBufferPointer {
    var span = MutableSpan(_unsafeElements: $0)

    var sub = span._mutatingExtracting(0..<2)
    expectEqual(sub.count, 2)
    expectEqual(sub[0], 0)

    sub = span._mutatingExtracting(..<2)
    expectEqual(sub.count, 2)
    expectEqual(sub[0], 0)

    sub = span._mutatingExtracting(...)
    expectEqual(sub.count, 4)
    expectEqual(sub[0], 0)

    sub = span._mutatingExtracting(2...)
    expectEqual(sub.count, 2)
    expectEqual(sub[0], 2)
  }
}

suite.test("_consumingExtracting()")
.require(.stdlib_6_2).code {

  let c = 16
  let b = UnsafeMutableBufferPointer<Int8>.allocate(capacity: c)
  defer { b.deallocate() }
  _ = b.initialize(fromContentsOf: 0..<Int8(c))

  var span = b.mutableSpan
  span = span._consumingExtracting(0..<2)
  expectEqual(span.count, 2)
  expectEqual(span[0], 0)

  span = b.mutableSpan
  span = span._consumingExtracting(..<2)
  expectEqual(span.count, 2)
  expectEqual(span[0], 0)

  span = b.mutableSpan
  span = span._consumingExtracting(...)
  expectEqual(span.count, c)
  expectEqual(span[0], 0)

  span = b.mutableSpan
  span = span._consumingExtracting(2...)
  expectEqual(span.count, c-2)
  expectEqual(span[0], 2)
}

suite.test("_mutatingExtracting(unchecked:)")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 32
  var b = (0..<capacity).map(UInt8.init)
  b.withUnsafeMutableBufferPointer {
    var span = MutableSpan(_unsafeElements: $0.prefix(8))
    let beyond = span._mutatingExtracting(unchecked: 16...23)
    expectEqual(beyond.count, 8)
    let fromBeyond = beyond[0]
    expectEqual(fromBeyond, 16)
  }
}

suite.test("_consumingExtracting(unchecked:)")
.require(.stdlib_6_2).code {

  let capacity = 32
  var b = (0..<capacity).map(UInt8.init)
  b.withUnsafeMutableBufferPointer {
    let span = MutableSpan(_unsafeElements: $0.prefix(8))
    let beyond = span._consumingExtracting(unchecked: 16...23)
    expectEqual(beyond.count, 8)
    expectEqual(beyond[0], 16)
  }
}

suite.test("_mutatingExtracting prefixes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBufferPointer {
    var prefix: MutableSpan<UInt8>
    var span = MutableSpan(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    prefix = span._mutatingExtracting(first: 1)
    expectEqual(prefix[0], 0)

    prefix = span._mutatingExtracting(first: capacity)
    expectEqual(prefix[capacity-1], UInt8(capacity-1))

    prefix = span._mutatingExtracting(droppingLast: capacity)
    expectEqual(prefix.isEmpty, true)

    prefix = span._mutatingExtracting(droppingLast: 1)
    expectEqual(prefix[capacity-2], UInt8(capacity-2))
  }

  do {
    let b = UnsafeMutableBufferPointer<Int>(start: nil, count: 0)
    var span = MutableSpan(_unsafeElements: b)
    expectEqual(span.count, b.count)
    expectEqual(span._mutatingExtracting(first: 1).count, b.count)
    expectEqual(span._mutatingExtracting(droppingLast: 1).count, b.count)
  }
}

suite.test("_consumingExtracting prefixes")
.require(.stdlib_6_2).code {

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeMutableBufferPointer {
    var span = $0.mutableSpan
    expectEqual(span.count, capacity)

    span = span._consumingExtracting(first: 1)
    expectEqual(span.count, 1)
    expectEqual(span[0], 0)

    span = $0.mutableSpan._consumingExtracting(first: capacity)
    expectEqual(span[capacity-1], capacity-1)

    span = $0.mutableSpan._consumingExtracting(droppingLast: capacity)
    expectEqual(span.isEmpty, true)

    span = $0.mutableSpan._consumingExtracting(droppingLast: 1)
    expectEqual(span[capacity-2], capacity-2)
  }

  do {
    let b = UnsafeMutableBufferPointer<Int>(start: nil, count: 0)
    var span = b.mutableSpan
    expectEqual(span.count, b.count)
    span = b.mutableSpan._consumingExtracting(first: 1)
    expectEqual(span.count, b.count)
    span = b.mutableSpan._consumingExtracting(droppingLast: 1)
    expectEqual(span.count, b.count)
  }
}

suite.test("_mutatingExtracting suffixes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  var a = Array(0..<UInt8(capacity))
  a.withUnsafeMutableBufferPointer {
    var suffix: MutableSpan<UInt8>
    var span = MutableSpan(_unsafeElements: $0)
    expectEqual(span.count, capacity)

    suffix = span._mutatingExtracting(last: capacity)
    expectEqual(suffix[0], 0)

    suffix = span._mutatingExtracting(last: capacity-1)
    expectEqual(suffix[0], 1)

    suffix = span._mutatingExtracting(last: 1)
    expectEqual(suffix[0], UInt8(capacity-1))

    suffix = span._mutatingExtracting(droppingFirst: capacity)
    expectTrue(suffix.isEmpty)

    suffix = span._mutatingExtracting(droppingFirst: 1)
    expectEqual(suffix[0], 1)
  }

  do {
    let b = UnsafeMutableBufferPointer<ObjectIdentifier>(start: nil, count: 0)
    var span = MutableSpan(_unsafeElements: b)
    expectEqual(span.count, b.count)
    expectEqual(span._mutatingExtracting(last: 1).count, b.count)
    expectEqual(span._mutatingExtracting(droppingFirst: 1).count, b.count)
  }
}

suite.test("_consumingExtracting suffixes")
.require(.stdlib_6_2).code {

  let capacity = 4
  var a = Array(0..<capacity)
  a.withUnsafeMutableBufferPointer {
    var span = $0.mutableSpan
    expectEqual(span.count, capacity)

    span = span._consumingExtracting(last: capacity)
    expectEqual(span[0], 0)

    span = $0.mutableSpan._consumingExtracting(last: capacity-1)
    expectEqual(span[0], 1)

    span = $0.mutableSpan._consumingExtracting(last: 1)
    expectEqual(span[0], capacity-1)

    span = $0.mutableSpan._consumingExtracting(droppingFirst: capacity)
    expectEqual(span.isEmpty, true)

    span = $0.mutableSpan._consumingExtracting(droppingFirst: 1)
    expectEqual(span[0], 1)
  }

  do {
    let b = UnsafeMutableBufferPointer<AnyObject>(start: nil, count: 0)
    var span = b.mutableSpan
    expectEqual(span.count, b.count)
    span = b.mutableSpan._consumingExtracting(last: 1)
    expectEqual(span.count, b.count)
    span = b.mutableSpan._consumingExtracting(droppingFirst: 1)
    expectEqual(span.count, b.count)
  }
}

suite.test("MutableSpan from UnsafeMutableBufferPointer")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let capacity = 4
  let b = UnsafeMutableBufferPointer<Int>.allocate(capacity: capacity)
  defer {
    b.deallocate()
  }
  _ = b.initialize(fromContentsOf: 0..<capacity)

  var span = b.mutableSpan
  expectEqual(span.count, capacity)

  span.swapAt(0, 3)
  span.swapAt(1, 2)

  _ = consume span

  expectTrue(b.elementsEqual((0..<capacity).reversed()))
}
