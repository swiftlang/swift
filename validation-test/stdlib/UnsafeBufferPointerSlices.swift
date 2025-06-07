// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// END.

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
    expectEqual(mb.reduce(Int.zero,+), t+5)
    let sb: Slice = mb[...]
    sb.withMemoryRebound(to: (UInt, UInt).self) {
      for i in $0.indices {
        $0[i].1 -= 1
      }
    }
    expectEqual(mb.reduce(Int.zero,+), t)
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
  expectEqual(mb.last.map(Int.init), c-1)
  var rb = mb.deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)

  (it, ct) = mb[...].initialize(from: (0..<c).map(String.init))
  expectNil(it.next())
  expectEqual(ct, c)
  expectEqual(mb.last.map(Int.init), c-1)
  rb = mb[...].deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)
}

UnsafeMutableBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableBufferPointer.initialize.fromContentsOf.Collection"
) {
  let c = 4
  let mb = UnsafeMutableBufferPointer<String>.allocate(capacity: c)
  defer { mb.deallocate() }

  var ct = mb.initialize(fromContentsOf: (1...c).map(String.init))
  expectEqual(ct, c)
  expectEqual(mb.last.map(Int.init), c)
  var rb = mb.deinitialize()
  expectEqual(rb.count, c*MemoryLayout<String>.stride)

  ct = mb[...].initialize(fromContentsOf: (1...c).map(String.init))
  expectEqual(ct, c)
  expectEqual(mb.last.map(Int.init), c)
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
    $0.update(fromContentsOf: (0..<c).map(String.init))
  }
  expectEqual(i, c)
  expectEqual(a.last.map(Int.init), c-1)

  i = b.withContiguousMutableStorageIfAvailable {
    $0[...].update(fromContentsOf: (0..<c).map(String.init))
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

  var i = buffer.initialize(fromContentsOf: source)
  expectEqual(i, c)
  i = a.moveInitialize(fromContentsOf: buffer)
  expectEqual(i, c)
  expectTrue(a.elementsEqual(source))
  i = buffer.initialize(fromContentsOf: source)
  expectEqual(i, c)
  i = b[...].moveInitialize(fromContentsOf: buffer)
  expectEqual(i, c)
  expectTrue(b.elementsEqual(source))

  i = buffer.initialize(fromContentsOf: source.prefix(n))
  expectEqual(i, n)
  i = a.moveInitialize(fromContentsOf: buffer.prefix(n))
  expectEqual(i, n)
  expectTrue(a[..<n].elementsEqual(source.prefix(n)))
  i = buffer.initialize(fromContentsOf: source.prefix(n))
  expectEqual(i, n)
  i = b[...].moveInitialize(fromContentsOf: buffer[..<n])
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
  i = buffer.initialize(fromContentsOf: source)
  expectEqual(i, c)
  i = a.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromContentsOf: buffer)
  }
  expectEqual(i, c)
  expectEqual(a, source)
  i = buffer.initialize(fromContentsOf: source)
  expectEqual(i, c)
  i = b.withContiguousMutableStorageIfAvailable {
    $0[...].moveUpdate(fromContentsOf: buffer)
  }
  expectEqual(i, c)
  expectEqual(a, b)

  i = buffer.initialize(fromContentsOf: source.prefix(n))
  expectEqual(i, n)
  i = a.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromContentsOf: buffer[..<n])
  }
  expectEqual(i, n)
  expectEqual(a[..<n], source[..<n])
  i = buffer.initialize(fromContentsOf: source.prefix(n))
  expectEqual(i, n)
  i = b.withContiguousMutableStorageIfAvailable {
    $0.moveUpdate(fromContentsOf: buffer[..<n])
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
  var i: Int? = a.initialize(fromContentsOf: Array(repeating: ".", count: c))
  defer { a.deinitialize() }
  expectEqual(i, c)
  expectTrue(a.allSatisfy({ $0 == "." }))

  i = a.withContiguousMutableStorageIfAvailable {
    $0.update(fromContentsOf: Array(repeating: " ", count: c))
  }
  expectEqual(i, c)
  expectTrue(a.allSatisfy({ $0 == " " }))

  i = a[...].withContiguousMutableStorageIfAvailable {
    $0.update(fromContentsOf: Array(repeating: "?", count: c))
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
  "slice.of.UnsafeMutableRawBufferpointer.initializeMemory.fromContentsOf"
) {
  let c = 4
  let mb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int>.stride * c,
    alignment: 16
  )
  defer { mb.deallocate() }

  var tb = mb.initializeMemory(as: Int.self, fromContentsOf: 0..<c)
  expectEqual(tb.count, c)
  expectTrue(tb.elementsEqual(0..<c))
  var rb = tb.deinitialize()
  expectEqual(rb.baseAddress, mb.baseAddress)
  expectEqual(rb.count, mb.count)

  tb = mb[...].initializeMemory(as: Int.self, fromContentsOf: 0..<c)
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

  expectEqual(buffer.initialize(fromContentsOf: source), c)
  var tba = rba.moveInitializeMemory(as: Int.self, fromContentsOf: buffer)
  expectEqual(tba.count, c)
  expectTrue(tba.elementsEqual(source))

  expectEqual(buffer.initialize(fromContentsOf: source), c)
  var tbb = rbb[...].moveInitializeMemory(as: Int.self, fromContentsOf: buffer)
  expectEqual(tbb.count, c)
  expectTrue(tbb.elementsEqual(tba))

  var dba = tba.deinitialize()
  var dbb = tbb.deinitialize()
  expectEqual(dba.count, rba.count)
  expectEqual(dbb.count, rbb.count)

  expectEqual(buffer.initialize(fromContentsOf: source.prefix(n)), n)
  tba = rba.moveInitializeMemory(as: Int.self, fromContentsOf: buffer.prefix(n))
  expectEqual(tba.count, n)
  expectTrue(tba.elementsEqual(source.prefix(n)))

  expectEqual(buffer.initialize(fromContentsOf: source.prefix(n)), n)
  tbb = rbb[...].moveInitializeMemory(as: Int.self, fromContentsOf: buffer[..<n])
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

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.load"
) {
  let count = 4
  let sizeInBytes = count * MemoryLayout<Int>.stride
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: sizeInBytes, alignment: MemoryLayout<Int>.alignment
  )
  defer {
    b.deallocate()
  }
  b.withMemoryRebound(to: Int.self) {
    var (i, c) = $0.update(from: 1...count)
    expectEqual(c, count)
    expectNil(i.next())
  }
  expectEqual(
    1, b[...].load(as: Int.self)
  )
  expectEqual(
    2, b[...].load(fromByteOffset: MemoryLayout<Int>.stride, as: Int.self)
  )
  expectEqual(
    3, b[MemoryLayout<Int>.stride...].load(
      fromByteOffset: MemoryLayout<Int>.stride, as: Int.self
    )
  )
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.loadUnaligned"
) {
  let sizeInBytes = 32
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: sizeInBytes, alignment: 4
  )
  defer {
    b.deallocate()
  }
  b.copyBytes(from: 0..<UInt8(sizeInBytes))
  let i = b[...].loadUnaligned(fromByteOffset: 3, as: Int.self)
  expectEqual(3, withUnsafeBytes(of: i, \.first))
  let i16 = b[7...].loadUnaligned(as: Int16.self)
  expectEqual(7, withUnsafeBytes(of: i16, \.first))
  let i32 = b[17...].loadUnaligned(fromByteOffset: 6, as: Int32.self)
  expectEqual(23, withUnsafeBytes(of: i32, \.first))
}

UnsafeMutableRawBufferPointerSliceTests.test(
  "slice.of.UnsafeMutableRawBufferPointer.storeBytes"
) {
  let count = 4
  let sizeInBytes = count * MemoryLayout<Int>.stride
  let b = UnsafeMutableRawBufferPointer.allocate(
    byteCount: sizeInBytes, alignment: MemoryLayout<Int>.alignment
  )
  defer {
    b.deallocate()
  }
  b.copyBytes(from: repeatElement(0, count: sizeInBytes))
  expectEqual(b[0], 0)
  b[...].storeBytes(of: .max, as: UInt8.self)
  expectEqual(b[0], .max)
  expectTrue(b[3..<3+MemoryLayout<UInt>.size].allSatisfy({ $0 == 0 }))
  b[3...].storeBytes(of: .max, toByteOffset: 4, as: UInt.self)
  expectTrue(b[7..<7+MemoryLayout<UInt>.size].allSatisfy({ $0 == .max }))
}

runAllTests()
