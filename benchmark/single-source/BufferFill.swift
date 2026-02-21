//===--- BufferFill.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "BufferFillFromSlice",
                runFunction: bufferFillFromSliceExecute,
                tags: [.validation, .api],
                setUpFunction: bufferFillFromSliceSetup,
                tearDownFunction: bufferFillFromSliceTeardown),
  BenchmarkInfo(name: "RawBufferCopyBytes",
                runFunction: rawBufferCopyBytesExecute,
                tags: [.validation, .api],
                setUpFunction: rawBufferCopyBytesSetup,
                tearDownFunction: rawBufferCopyBytesTeardown),
  BenchmarkInfo(name: "RawBufferInitializeMemory",
                runFunction: rawBufferInitializeMemoryExecute,
                tags: [.validation, .api],
                setUpFunction: rawBufferInitializeMemorySetup,
                tearDownFunction: rawBufferInitializeMemoryTeardown),
  BenchmarkInfo(name: "RawBuffer.copyContents",
                runFunction: rawBufferCopyContentsExecute,
                tags: [.validation, .api],
                setUpFunction: rawBufferCopyContentsSetup,
                tearDownFunction: rawBufferCopyContentsTeardown),
]

let c = 100_000
let a = Array(0..<c)
var b: UnsafeMutableBufferPointer<Int> = .init(start: nil, count: 0)
var r = Int.zero

public func bufferFillFromSliceSetup() {
  assert(b.baseAddress == nil)
  b = .allocate(capacity: c)
  r = a.indices.randomElement()!
}

public func bufferFillFromSliceTeardown() {
  b.deallocate()
  b = .init(start: nil, count: 0)
}

@inline(never)
public func bufferFillFromSliceExecute(n: Int) {
  // Measure performance when filling an UnsafeBuffer from a Slice
  // of a Collection that supports `withContiguousStorageIfAvailable`
  // See: https://github.com/apple/swift/issues/56846

  for _ in 0..<n {
    let slice = Slice(base: a, bounds: a.indices)
    var (iterator, copied) = b.initialize(from: slice)
    blackHole(b)
    check(copied == a.count && iterator.next() == nil)
  }

  check(a[r] == b[r])
}

var ra: [UInt8] = []
var rb = UnsafeMutableRawBufferPointer(start: nil, count: 0)

public func rawBufferCopyBytesSetup() {
  assert(rb.baseAddress == nil)
  ra = a.withUnsafeBytes(Array.init)
  r = ra.indices.randomElement()!
  rb = .allocate(byteCount: ra.count, alignment: 1)
}

public func rawBufferCopyBytesTeardown() {
  rb.deallocate()
  rb = .init(start: nil, count: 0)
  ra = []
}

@inline(never)
public func rawBufferCopyBytesExecute(n: Int) {
  // Measure performance when copying bytes into an UnsafeRawBuffer
  // from a Collection that supports `withContiguousStorageIfAvailable`
  // See: https://github.com/apple/swift/issues/57233

  for _ in 0..<n {
    rb.copyBytes(from: ra)
    blackHole(rb)
  }

  check(ra[r] == rb[r])
}

public func rawBufferInitializeMemorySetup() {
  assert(rb.baseAddress == nil)
  rb = .allocate(byteCount: a.count * MemoryLayout<Int>.stride, alignment: 1)
  r = a.indices.randomElement()!
}

public func rawBufferInitializeMemoryTeardown() {
  rb.deallocate()
  rb = .init(start: nil, count: 0)
}

@inline(never)
public func rawBufferInitializeMemoryExecute(n: Int) {
  // Measure performance when initializing an UnsafeRawBuffer
  // from a Collection that supports `withContiguousStorageIfAvailable`
  // See: https://github.com/apple/swift/issues/57324

  for _ in 0..<n {
    var (iterator, initialized) = rb.initializeMemory(as: Int.self, from: a)
    blackHole(rb)
    check(initialized.count == a.count && iterator.next() == nil)
  }

  let offset = rb.baseAddress!.advanced(by: r*MemoryLayout<Int>.stride)
  let value = offset.load(as: Int.self)
  check(value == a[r])
}

var r8: UnsafeRawBufferPointer = .init(start: nil, count: 0)
var b8: UnsafeMutableBufferPointer<UInt8> = .init(start: nil, count: 0)

public func rawBufferCopyContentsSetup() {
  assert(r8.baseAddress == nil)
  let count = a.count * MemoryLayout<Int>.stride
  let rb = UnsafeMutableRawBufferPointer.allocate(
    byteCount: count, 
    alignment: MemoryLayout<Int>.alignment)
  a.withUnsafeBytes {
    rb.copyMemory(from: $0)
  }
  r8 = UnsafeRawBufferPointer(rb)
  assert(b8.baseAddress == nil)
  b8 = .allocate(capacity: rb.count)
  r = rb.indices.randomElement()!
}

public func rawBufferCopyContentsTeardown() {
  r8.deallocate()
  r8 = .init(start: nil, count: 0)
  b8.deallocate()
  b8 = .init(start: nil, count: 0)
}

@inline(never)
public func rawBufferCopyContentsExecute(n: Int) {
  // Measure performance of copying bytes from an
  // `UnsafeRawBufferPointer` to an `UnsafeMutableBufferPointer<UInt8>`.
  // See: https://github.com/apple/swift/issues/52050

  for _ in 0..<n {
    var (iterator, initialized) = b8.initialize(from: r8)
    blackHole(b8)
    check(initialized == r8.count && iterator.next() == nil)
  }

  check(b8[r] == r8[r])
}
