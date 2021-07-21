//===--- BufferFill.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let BufferFill = [
  BenchmarkInfo(name: "BufferFillFromSlice",
                runFunction: BufferFillFromSliceExecute,
                tags: [.validation, .api],
                setUpFunction: BufferFillFromSliceSetup,
                tearDownFunction: BufferFillFromSliceTeardown),
  BenchmarkInfo(name: "RawBufferCopyBytes",
                runFunction: RawBufferCopyBytesExecute,
                tags: [.validation, .api],
                setUpFunction: RawBufferCopyBytesSetup,
                tearDownFunction: RawBufferCopyBytesTeardown)
]

let c = 50_000
let a = Array(0..<c)
var b: UnsafeMutableBufferPointer<Int> = .init(start: nil, count: 0)

public func BufferFillFromSliceSetup() {
  assert(b.baseAddress == nil)
  b = .allocate(capacity: c)
}

public func BufferFillFromSliceTeardown() {
  b.deallocate()
  b = .init(start: nil, count: 0)
}

@inline(never)
public func BufferFillFromSliceExecute(N: Int) {
  // Measure performance when filling an UnsafeBuffer from a Slice
  // See: https://bugs.swift.org/browse/SR-14491

  for _ in 0..<N {
    let slice = Slice(base: a, bounds: 0..<c)
    let (_, n) = b.initialize(from: slice)
    guard n == c else { break }
  }

  let r = a.indices.randomElement()!
  CheckResults(a[r] == b[r])
}

var ra: [UInt8] = []
var rb = UnsafeMutableRawBufferPointer(start: nil, count: 0)

public func RawBufferCopyBytesSetup() {
  assert(rb.baseAddress == nil)

  ra = a.withUnsafeBytes(Array.init)
  rb = .allocate(byteCount: ra.count, alignment: 1)
}

public func RawBufferCopyBytesTeardown() {
  rb.deallocate()
  rb = .init(start: nil, count: 0)
  ra = []
}

@inline(never)
public func RawBufferCopyBytesExecute(N: Int) {
  // Measure performance when filling an UnsafeRawBuffer from a Collection
  // See: https://bugs.swift.org/browse/SR-14886

  for i in 0..<N {
    rb.copyBytes(from: ra)
  }

  let r = ra.indices.randomElement()!
  assert(ra[r] == rb[r])
}

@inline(never)
public func read(_ b: UnsafeMutableRawBufferPointer, _ i: Int) -> UInt8{
    return b[i]
}
