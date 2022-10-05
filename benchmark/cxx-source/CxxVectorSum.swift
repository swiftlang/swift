//===--- CxxVectorSum.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2012 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This is a benchmark that tracks how quickly Swift can sum up a C++ vector
// as compared to the C++ implementation of such sum.

import TestsUtils
import CxxStdlibPerformance
import Cxx

public let benchmarks = [
  BenchmarkInfo(
      name: "CxxVectorOfU32SumInCxx",
      runFunction: run_CxxVectorOfU32SumInCxx,
      tags: [.validation, .bridging, .cxxInterop],
      setUpFunction: create_CxxVectorOfU32),
  BenchmarkInfo(
    name: "CxxVectorOfU32SumInSwift_Fastest",
    runFunction: run_CxxVectorOfU32SumInSwift_Fastest,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: create_CxxVectorOfU32),
  BenchmarkInfo(
    name: "CxxVectorOfU32SumInSwift",
    runFunction: run_CxxVectorOfU32SumInSwift,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: create_CxxVectorOfU32)
]

// FIXME: compare CxxVectorOfU32SumInCxx to CxxVectorOfU32SumInSwift and
// establish an expected threshold of performance, which when exceeded should
// fail the benchmark.

var vectorOfU32: VectorOfU32!

func create_CxxVectorOfU32() {
    vectorOfU32 = makeVector32(1_000_000)
}

@inline(never)
public func run_CxxVectorOfU32SumInCxx(_ n: Int) {
  // FIXME: Use no implicitly copyable, immutable borrow instead.
  // https://github.com/apple/swift/issues/61454
  let sum = testVector32Sum(&vectorOfU32)
  blackHole(sum)
}

// This function should have comparable performance to `run_CxxVectorOfU32SumInCxx`.
@inline(never)
public func run_CxxVectorOfU32SumInSwift_Fastest(_ n: Int) {
  var sum: UInt32 = 0
  // begin mutating, end mutating needed to avoid copies right now.
  var b = vectorOfU32.beginMutating()
  let e = vectorOfU32.endMutating()
  while !cmp(b, e) {
    sum = sum &+ b.pointee
    b = next(b)
  }
  blackHole(sum)
}

public func !=(_ y: VectorOfU32.const_iterator, _ x: VectorOfU32.const_iterator) -> Bool {
    return y.__baseUnsafe() != x.__baseUnsafe()
}

public func ==(_ y: VectorOfU32.const_iterator, _ x: VectorOfU32.const_iterator) -> Bool {
    return y.__baseUnsafe() == x.__baseUnsafe()
}

extension VectorOfU32.const_iterator : Equatable, UnsafeCxxInputIterator { }

extension VectorOfU32: CxxSequence {}

@inline(never)
public func run_CxxVectorOfU32SumInSwift(_ n: Int) {
    var sum: UInt32 = 0
    for i in vectorOfU32 {
        sum = sum &+ i
    }
    blackHole(sum)
}
