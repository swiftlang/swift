//===--- SIMDReduceInteger.swift ------------------------------------------===//
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

public let SIMDReduceInteger = [
  BenchmarkInfo(
    name: "SIMDReduce.Int32",
    runFunction: run_SIMDReduceInt32x1,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int32Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int32x4.Initializer",
    runFunction: run_SIMDReduceInt32x4_init,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int32Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int32x4.Cast",
    runFunction: run_SIMDReduceInt32x4_cast,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int32Data) }
  ), 
  BenchmarkInfo(
    name: "SIMDReduce.Int32x16.Initializer",
    runFunction: run_SIMDReduceInt32x16_init,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int32Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int32x16.Cast",
    runFunction: run_SIMDReduceInt32x16_cast,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int32Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int8",
    runFunction: run_SIMDReduceInt8x1,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int8Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int8x16.Initializer",
    runFunction: run_SIMDReduceInt8x16_init,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int8Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int8x16.Cast",
    runFunction: run_SIMDReduceInt8x16_cast,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int8Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int8x64.Initializer",
    runFunction: run_SIMDReduceInt8x64_init,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int8Data) }
  ),
  BenchmarkInfo(
    name: "SIMDReduce.Int8x64.Cast",
    runFunction: run_SIMDReduceInt8x64_cast,
    tags: [.validation, .SIMD],
    setUpFunction: { blackHole(int8Data) }
  )
]

// TODO: use 100 for Onone?
let scale = 1000

let int32Data: UnsafeBufferPointer<Int32> = {
  let count = 256
  // Allocate memory for `count` Int32s with alignment suitable for all
  // SIMD vector types.
  let untyped = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int32>.size * count, alignment: 16
  )
  // Intialize the memory as Int32 and fill with random values.
  let typed = untyped.initializeMemory(as: Int32.self, repeating: 0)
  var g = SplitMix64(seed: 0)
  for i in 0 ..< typed.count {
    typed[i] = .random(in: .min ... .max, using: &g)
  }
  return UnsafeBufferPointer(typed)
}()

@inline(never)
public func run_SIMDReduceInt32x1(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum: Int32 = 0
    for v in int32Data {
      accum &+= v &* v
    }
    blackHole(accum)
  }
}

@inline(never)
public func run_SIMDReduceInt32x4_init(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum = SIMD4<Int32>()
    for i in stride(from: 0, to: int32Data.count, by: 4) {
      let v = SIMD4(int32Data[i ..< i+4])
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt32x4_cast(_ N: Int) {
  // Morally it seems like we "should" be able to use withMemoryRebound
  // to SIMD4<Int32>, but that function requries that the sizes match in
  // debug builds, so this is pretty ugly. The following "works" for now,
  // but is probably in violation of the formal model (the exact rules
  // for "assumingMemoryBound" are not clearly documented). We need a
  // better solution.
  let vecs = UnsafeBufferPointer<SIMD4<Int32>>(
    start: UnsafeRawPointer(int32Data.baseAddress!).assumingMemoryBound(to: SIMD4<Int32>.self),
    count: int32Data.count / 4
  )
  for _ in 0 ..< scale*N {
    var accum = SIMD4<Int32>()
    for v in vecs {
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt32x16_init(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum = SIMD16<Int32>()
    for i in stride(from: 0, to: int32Data.count, by: 16) {
      let v = SIMD16(int32Data[i ..< i+16])
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt32x16_cast(_ N: Int) {
  let vecs = UnsafeBufferPointer<SIMD16<Int32>>(
    start: UnsafeRawPointer(int32Data.baseAddress!).assumingMemoryBound(to: SIMD16<Int32>.self),
    count: int32Data.count / 16
  )
  for _ in 0 ..< scale*N {
    var accum = SIMD16<Int32>()
    for v in vecs {
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

let int8Data: UnsafeBufferPointer<Int8> = {
  let count = 1024
  // Allocate memory for `count` Int8s with alignment suitable for all
  // SIMD vector types.
  let untyped = UnsafeMutableRawBufferPointer.allocate(
    byteCount: MemoryLayout<Int8>.size * count, alignment: 16
  )
  // Intialize the memory as Int8 and fill with random values.
  let typed = untyped.initializeMemory(as: Int8.self, repeating: 0)
  var g = SplitMix64(seed: 0)
  for i in 0 ..< typed.count {
    typed[i] = .random(in: .min ... .max, using: &g)
  }
  return UnsafeBufferPointer(typed)
}()

@inline(never)
public func run_SIMDReduceInt8x1(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum: Int8 = 0
    for v in int8Data {
      accum &+= v &* v
    }
    blackHole(accum)
  }
}

@inline(never)
public func run_SIMDReduceInt8x16_init(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum = SIMD16<Int8>()
    for i in stride(from: 0, to: int8Data.count, by: 16) {
      let v = SIMD16(int8Data[i ..< i+16])
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt8x16_cast(_ N: Int) {
  let vecs = UnsafeBufferPointer<SIMD16<Int8>>(
    start: UnsafeRawPointer(int8Data.baseAddress!).assumingMemoryBound(to: SIMD16<Int8>.self),
    count: int8Data.count / 16
  )
  for _ in 0 ..< scale*N {
    var accum = SIMD16<Int8>()
    for v in vecs {
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt8x64_init(_ N: Int) {
  for _ in 0 ..< scale*N {
    var accum = SIMD64<Int8>()
    for i in stride(from: 0, to: int8Data.count, by: 64) {
      let v = SIMD64(int8Data[i ..< i+64])
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}

@inline(never)
public func run_SIMDReduceInt8x64_cast(_ N: Int) {
  let vecs = UnsafeBufferPointer<SIMD64<Int8>>(
    start: UnsafeRawPointer(int8Data.baseAddress!).assumingMemoryBound(to: SIMD64<Int8>.self),
    count: int8Data.count / 64
  )
  for _ in 0 ..< scale*N {
    var accum = SIMD64<Int8>()
    for v in vecs {
      accum &+= v &* v
    }
    blackHole(accum.wrappedSum())
  }
}
