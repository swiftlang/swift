// RUN: %target-run-simple-swiftgyb
// REQUIRES: executable_test

import StdlibUnittest

var UnsafeBufferPointerSliceTests = TestSuite(
  "UnsafeBufferPointerSliceTests"
)

UnsafeBufferPointerSliceTests.test(
  "slice.of.UnsafeBufferPointer.withMemoryRebound"
) {
  let a = Array(0..<10)
  let r: Bool? = a.withContiguousStorageIfAvailable {
    let b: UnsafeBufferPointer = $0
    var c: UInt
    c = b.withMemoryRebound(to: (UInt, UInt).self) {
      $0.reduce(0, { $0 + $1.1 })
    }
    expectEqual(c, 25)
    let s = b[...]
    c = s.withMemoryRebound(to: (UInt, UInt).self) {
      $0.reduce(0, { $0 + $1.1 })
    }
    expectEqual(c, 25)
    return true
  }
  expectNotNil(r)
}

var UnsafeMutableBufferPointerSliceTests = TestSuite(
  "UnsafeMutableBufferPointerSliceTests"
)

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.withMemoryRebound"
) {
  var a = Array(0..<10)
  let t = a.reduce(0,+)
  let r: Bool? = a.withContiguousMutableStorageIfAvailable {
    let mb: UnsafeMutableBufferPointer = $0
    mb.withMemoryRebound(to: (UInt, UInt).self) {
      for i in $0.indices {
        $0[i].0 += 1
      }
    }
    expectEqual(mb.reduce(0,+), t+5)
    let sb: Slice = mb[...]
    sb.withMemoryRebound(to: (UInt, UInt).self) {
      for i in $0.indices {
        $0[i].1 -= 1
      }
    }
    expectEqual(mb.reduce(0,+), t)
    return true
  }
  expectNotNil(r)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.initialize.repeating"
) {
  let c = 4
  let mb = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { mb.deallocate() }

  mb.initialize(repeating: "0")
  expectTrue(mb.allSatisfy({ $0 == "0" }))
  var rb = mb.deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)

  mb.initializeElement(at: 0, to: "0")
  mb[1...].initialize(repeating: mb[0])
  expectTrue(mb.allSatisfy({ $0 == "0" }))
  rb = mb[...].deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.initialize.from.Sequence"
) {
  let c = 4
  let mb = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { mb.deallocate() }

  var (it, ct) = mb.initialize(from: (0..<c).map(String.init))
  expectNil(it.next())
  expectEqual(ct, c)
  expectEqual(mb.compactMap(Int.init).reduce(0,+), c*(c-1)/2)
  var rb = mb.deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)

  (it, ct) = mb[...].initialize(from: (0..<c).map(String.init))
  expectNil(it.next())
  expectEqual(ct, c)
  expectEqual(mb.compactMap(Int.init).reduce(0,+), c*(c-1)/2)
  rb = mb[...].deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.initialize.fromElements.Collection"
) {
  let c = 4
  let mb = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { mb.deallocate() }

  var ct = mb.initialize(fromElements: (1...c).map(String.init))
  expectEqual(ct, c)
  expectEqual(mb.compactMap(Int.init).reduce(0,+), c*(c+1)/2)
  var rb = mb.deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)

  ct = mb[...].initialize(fromElements: (1...c).map(String.init))
  expectEqual(ct, c)
  expectEqual(mb.compactMap(Int.init).reduce(0,+), c*(c+1)/2)
  rb = mb[...].deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.update"
) {
  let c = 4
  var a = Array(repeating: " ", count: c)
  var b = a

  var r: Void? = a.withContiguousMutableStorageIfAvailable {
    $0.update(repeating: "")
  }
  expectNotNil(r)
  expectTrue(a.allSatisfy(\.isEmpty))

  r = b.withContiguousMutableStorageIfAvailable {
    $0[...].update(repeating: "")
  }
  expectNotNil(r)
  expectEqual(a, b)

  var itAndCount = a.withContiguousMutableStorageIfAvailable {
    $0.update(from: Array(repeating: ".", count: c))
  }
  expectNotNil(itAndCount)
  expectNil(itAndCount!.0.next())
  expectEqual(itAndCount?.1, c)

  itAndCount = b.withContiguousMutableStorageIfAvailable {
    $0[...].update(from: Array(repeating: ".", count: c))
  }
  expectNotNil(itAndCount)
  expectNil(itAndCount!.0.next())
  expectEqual(a, b)

  var i = a.withContiguousMutableStorageIfAvailable {
    $0.update(fromElements: (0..<c).map(String.init))
  }
  expectEqual(i, c)
  expectEqual(a.compactMap(Int.init).reduce(0,+), c*(c-1)/2)

  i = b.withContiguousMutableStorageIfAvailable {
    $0[...].update(fromElements: (0..<c).map(String.init))
  }
  expectEqual(i, c)
  expectEqual(a, b)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.moveInitialize"
) {
  let c = 4
  let n = c-1

  let source = (0..<c).map(String.init)
  let buffer = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { buffer.deallocate() }

  let a = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { a.deallocate() }
  let b = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { b.deallocate() }

  var i = buffer.initialize(fromElements: source)
  expectEqual(i, c)
  i = a.moveInitialize(fromElements: buffer)
  expectEqual(i, c)
  expectTrue(a.elementsEqual(source))
  i = buffer.initialize(fromElements: source)
  expectEqual(i, c)
  i = b[...].moveInitialize(fromElements: buffer)
  expectEqual(i, c)
  expectTrue(b.elementsEqual(source))

  i = buffer.initialize(fromElements: source.prefix(n))
  expectEqual(i, n)
  i = a.moveInitialize(fromElements: buffer.prefix(n))
  expectEqual(i, n)
  expectTrue(a[..<n].elementsEqual(source.prefix(n)))
  i = buffer.initialize(fromElements: source.prefix(n))
  expectEqual(i, n)
  i = b[...].moveInitialize(fromElements: buffer[..<n])
  expectEqual(i, n)
  expectTrue(b.prefix(n).elementsEqual(a[..<n]))
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.moveUpdate"
) {
  let c = 4
  let n = c-1

  let source = (0..<c).map(String.init)
  let buffer = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { buffer.deallocate() }

  var a = Array(repeating: "", count: c)
  var b = a

  var i: Int?
  i = buffer.initialize(fromElements: source)
  expectEqual(i, c)
  i = a.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromElements: buffer)
  }
  expectEqual(i, c)
  expectEqual(a, source)
  i = buffer.initialize(fromElements: source)
  expectEqual(i, c)
  i = b.withContiguousMutableStorageIfAvailable {
    $0[...].moveUpdate(fromElements: buffer)
  }
  expectEqual(i, c)
  expectEqual(a, b)

  i = buffer.initialize(fromElements: source.prefix(n))
  expectEqual(i, n)
  i = a.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromElements: buffer[..<n])
  }
  expectEqual(i, n)
  expectEqual(a[..<n], source[..<n])
  i = buffer.initialize(fromElements: source.prefix(n))
  expectEqual(i, n)
  i = b.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromElements: buffer[..<n])
  }
  expectEqual(i, n)
  expectEqual(a[..<n], b[..<n])
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.single.element.methods"
) {
  let c = 4
  let n = c-1
  let s = "Sample string with multiple words."
  let t = "Another string that is not short."

  let a = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  let b = UnsafeMutableBufferPointer<String>.allocate(capacity: c)

  a.initializeElement(at: n, to: s)
  expectEqual(a[n], s)
  b[...].initializeElement(at: n, to: s)
  expectEqual(b[n], s)

  expectEqual(a.moveElement(from: n), s)
  expectEqual(b[...].moveElement(from: n), s)

  a.initializeElement(at: 0, to: t)
  a.deinitializeElement(at: 0)
  b.initializeElement(at: 0, to: t)
  b[...].deinitializeElement(at: 0)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferpointer.withContiguousMutableStorageIfAvailable"
) {
  let c = 4

  var a = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { a.deallocate() }
  a.initialize(fromElements: Array(repeating: ".", count: c))
  defer { a.deinitialize() }

  var i = a.withContiguousMutableStorageIfAvailable {
    $0.update(fromElements: Array(repeating: " ", count: c))
  }
  expectEqual(i, c)
  expectTrue(a.allSatisfy({ $0 == " " }))

  i = a[...].withContiguousMutableStorageIfAvailable {
    $0.update(fromElements: Array(repeating: "?", count: c))
  }
  expectEqual(i, c)
  expectTrue(a.allSatisfy({ $0 == "?" }))
}

var UnsafeRawBufferPointerSliceTests = TestSuite(
  "UnsafeRawBufferPointerSliceTests"
)

UnsafeRawBufferPointerSliceTests.test(
  "slice.of.UnsafeRawBufferPointer.bindMemory"
) {
  let c = 4
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: c * MemoryLayout<Int>.stride,
    alignment: MemoryLayout<Int>.alignment
  )
  defer { b.deallocate() }

  let b1 = UnsafeRawBufferPointer(b).bindMemory(to: Int.self)
  var i = b1[0]
  expectType(Int.self, &i)
  let b2 = UnsafeRawBufferPointer(b)[...].bindMemory(to: Int.self)
  i = b2[0]
  expectType(Int.self, &i)
  expectEqual(b1.count, b2.count)
}

UnsafeRawBufferPointerSliceTests.test(
  "slice.of.UnsafeRawBufferPointer.withMemoryRebound"
) {
  let c = 4
  let m = UnsafeMutableRawBufferPointer.allocate(
    byteCount: c * MemoryLayout<Int>.stride,
    alignment: MemoryLayout<Int>.alignment
  )
  defer { m.deallocate() }

  let b = UnsafeRawBufferPointer(m)
  b.withMemoryRebound(to: Int.self) {
    var v = $0[0]
    expectType(Int.self, &v)
    expectEqual($0.count, c)
  }

  b[...].withMemoryRebound(to: Int.self, {
    var v = $0[0]
    expectType(Int.self, &v)
    expectEqual($0.count, c)
  })
}

UnsafeRawBufferPointerSliceTests.test(
  "slice.of.UnsafeRawBufferPointer.assumingMemoryBound"
) {
  let c = 4
  let array = Array(0..<c)
  array.withUnsafeBytes({
    var b: UnsafeBufferPointer = $0.assumingMemoryBound(to: Int.self)
    expectEqual(b.count, array.count)
    expectTrue(b.elementsEqual(array))

    b = $0[...].assumingMemoryBound(to: Int.self)
    expectEqual(b.count, array.count)
    expectTrue(b.elementsEqual(array))
  })
}

var UnsafeMutableRawBufferPointerSliceTests = TestSuite(
  "UnsafeMutableRawBufferPointerSliceTests"
)

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.copyBytes"
) {
  let c = UInt8(4)

  let source = Array<UInt8>(0..<c)

  var a = Array(repeating: UInt8.max, count: Int(4))
  var b = a

  a.withUnsafeMutableBytes { $0.copyBytes(from: source) }
  expectFalse(a.elementsEqual(b))

  b.withUnsafeMutableBytes { $0[...].copyBytes(from: source) }
  expectTrue(a.elementsEqual(b))
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.initializeMemory.repeating"
) {
  let c = 4
  let mb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { mb.deallocate() }

  var tb = mb.initializeMemory(as: Int.self, repeating: .min)
  expectEqual(tb.count, c)
  expectTrue(tb.allSatisfy({ $0 == .min }))
  var rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)

  tb = mb[...].initializeMemory(as: Int.self, repeating: 0)
  expectEqual(tb.count, c)
  expectTrue(tb.allSatisfy({ $0 == 0 }))
  rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.initializeMemory.from.Sequence"
) {
  let c = 4
  let mb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { mb.deallocate() }

  var (it, tb) = mb.initializeMemory(as: Int.self, from: 0..<c)
  expectNil(it.next())
  expectEqual(tb.count, c)
  expectTrue(tb.elementsEqual(0..<c))
  var rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)

  (it, tb) = mb[...].initializeMemory(as: Int.self, from: 0..<c)
  expectNil(it.next())
  expectEqual(tb.count, c)
  expectTrue(tb.elementsEqual(0..<c))
  rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferpointer.initializeMemory.fromElements"
) {
  let c = 4
  let mb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { mb.deallocate() }

  var tb = mb.initializeMemory(as: Int.self, fromElements: 0..<c)
  expectEqual(tb.count, c)
  expectTrue(tb.elementsEqual(0..<c))
  var rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)

  tb = mb[...].initializeMemory(as: Int.self, fromElements: 0..<c)
  expectEqual(tb.count, c)
  expectTrue(tb.elementsEqual(0..<c))
  rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferpointer.moveInitializeMemory"
) {
  let c = 4
  let n = c-1

  let source = 0..<c
  let buffer = UnsafeMutableBufferPointer<Int>.allocate(capacity: c)
  defer { buffer.deallocate() }

  let rba = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { rba.deallocate() }
  let rbb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { rbb.deallocate() }

  expectEqual(buffer.initialize(fromElements: source), c)
  var tba = rba.moveInitializeMemory(as: Int.self, fromElements: buffer)
  expectEqual(tba.count, c)
  expectTrue(tba.elementsEqual(source))

  expectEqual(buffer.initialize(fromElements: source), c)
  var tbb = rbb[...].moveInitializeMemory(as: Int.self, fromElements: buffer)
  expectEqual(tbb.count, c)
  expectTrue(tbb.elementsEqual(tba))

  var dba = tba.deinitialize()
  var dbb = tbb.deinitialize()
  expectEqual(dba.count, rba.count)
  expectEqual(dbb.count, rbb.count)

  expectEqual(buffer.initialize(fromElements: source.prefix(n)), n)
  tba = rba.moveInitializeMemory(as: Int.self, fromElements: buffer.prefix(n))
  expectEqual(tba.count, n)
  expectTrue(tba.elementsEqual(source.prefix(n)))

  expectEqual(buffer.initialize(fromElements: source.prefix(n)), n)
  tbb = rbb[...].moveInitializeMemory(as: Int.self, fromElements: buffer[..<n])
  expectEqual(tbb.count, n)
  expectTrue(tbb.elementsEqual(tba))

  dba = tba.deinitialize()
  dbb = tbb.deinitialize()
  expectEqual(dba.baseAddress, rba.baseAddress)
  expectEqual(dbb.baseAddress, rbb.baseAddress)
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.bindMemory"
) {
  let c = 4
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: c * MemoryLayout<Int>.stride,
    alignment: MemoryLayout<Int>.alignment
  )
  defer { b.deallocate() }

  let b1 = b.bindMemory(to: Int.self)
  expectType(Int.self, &b1[0])
  let b2 = b[...].bindMemory(to: Int.self)
  expectType(Int.self, &b2[0])
  expectEqual(b1.count, b2.count)
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.withMemoryRebound"
) {
  let c = 4
  let m = UnsafeMutableRawBufferPointer.allocate(
    byteCount: c * MemoryLayout<Int>.stride,
    alignment: MemoryLayout<Int>.alignment
  )
  defer { m.deallocate() }

  m.withMemoryRebound(to: Int.self) {
    expectType(Int.self, &$0[0])
    expectEqual($0.count, c)
  }

  m[...].withMemoryRebound(to: Int.self, {
    expectType(Int.self, &$0[0])
    expectEqual($0.count, c)
  })
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.assumingMemoryBound"
) {
  let c = 4
  let source = Array(0..<c)
  var array = source
  array.withUnsafeMutableBytes({
    var b: UnsafeMutableBufferPointer = $0.assumingMemoryBound(to: Int.self)
    expectEqual(b.count, source.count)
    expectTrue(b.elementsEqual(source))

    b = $0[...].assumingMemoryBound(to: Int.self)
    expectEqual(b.count, source.count)
    expectTrue(b.elementsEqual(source))
  })
}

runAllTests()
