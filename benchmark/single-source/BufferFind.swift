//===--- BufferFind.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public var benchmarks: [BenchmarkInfo] = [
  BenchmarkInfo(
    name: "RawBuffer.100000.findFirst",
    runFunction: run_BufferFindFirst,
    tags: [.validation, .api],
    setUpFunction: buffer100000Setup,
    tearDownFunction: bufferTeardown
  ),
  BenchmarkInfo(
    name: "RawBuffer.100000.findLast",
    runFunction: run_BufferFindLast,
    tags: [.validation, .api],
    setUpFunction: buffer100000Setup,
    tearDownFunction: bufferTeardown
  ),
  // size 1000, alignment 0
  BenchmarkInfo(
    name: "RawBuffer.1000.findFirst",
    runFunction: run_BufferFindFirst,
    tags: [.validation, .api],
    setUpFunction: buffer1000Setup,
    tearDownFunction: bufferTeardown
  ),
  BenchmarkInfo(
    name: "RawBuffer.1000.findLast",
    runFunction: run_BufferFindLast,
    tags: [.validation, .api],
    setUpFunction: buffer1000Setup,
    tearDownFunction: bufferTeardown
  ),
  // size 100, alignment 0
  BenchmarkInfo(
    name: "RawBuffer.100.findFirst",
    runFunction: run_BufferFindFirst,
    tags: [.validation, .api],
    setUpFunction: buffer100Setup,
    tearDownFunction: bufferTeardown
  ),
  BenchmarkInfo(
    name: "RawBuffer.100.findLast",
    runFunction: run_BufferFindLast,
    tags: [.validation, .api],
    setUpFunction: buffer100Setup,
    tearDownFunction: bufferTeardown
  ),
  // size 10, alignment 0
  BenchmarkInfo(
    name: "RawBuffer.10.findFirst",
    runFunction: run_BufferFindFirst,
    tags: [.validation, .api],
    setUpFunction: buffer10Setup,
    tearDownFunction: bufferTeardown
  ),
  BenchmarkInfo(
    name: "RawBuffer.10.findLast",
    runFunction: run_BufferFindLast,
    tags: [.validation, .api],
    setUpFunction: buffer10Setup,
    tearDownFunction: bufferTeardown
  )
]

var buffer: UnsafeMutableRawBufferPointer = .init(start: nil, count: 0)

func buffer100000Setup() {
  bufferSetup(size: 100_000, alignment: 0)
}

func buffer1000Setup() {
  bufferSetup(size: 1000, alignment: 0)
}

func buffer100Setup() {
  bufferSetup(size: 100, alignment: 0)
}

func buffer10Setup() {
  bufferSetup(size: 10, alignment: 0)
}

func bufferTeardown() {
  buffer.deallocate()
  buffer = .init(start: nil, count: 0)
}

func bufferSetup(size: Int, alignment: Int) {
  buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: size, alignment: alignment)
  buffer.initializeMemory(as: UInt8.self, repeating: UInt8.min)
  buffer[size / 2] = UInt8.max
}

@inline(never)
public func run_BufferFindFirst(n: Int) {
  var offset = 0
  for _ in 0 ..< n * 10 {
    if let index = buffer.firstIndex(of: UInt8.max) {
      offset += index
    }
  }
  blackHole(offset)
}

@inline(never)
public func run_BufferFindLast(n: Int) {
  var offset = 0
  for _ in 0 ..< n * 10 {
    if let index = buffer.lastIndex(of: UInt8.max) {
      offset += index
    }
  }
  blackHole(offset)
}
