//===--- ArrayRRC.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Darwin

fileprivate class DummyObject {}

fileprivate func makeRefcounted(count: Int) -> [DummyObject] {
  var result = (0 ..< count - 1).map { _ in DummyObject() }
  blackHole(result)
  result.append(DummyObject()) //force a resize if result was perfectly fitted
  return result
}

fileprivate func ranges(count: Int) -> [(Range<Int>, String)] {
  var results:[(Range<Int>, String)] = []
  results.append((0 ..< (count / 3), "head"))
  results.append((((count / 3) * 2) ..< count, "tail"))
  return results
}

fileprivate let sourceSizes = [(10_000, "lg"), (99, "sm")/*, (1, "xs")*/]
fileprivate let destSizes = [(10_000, "lg"), (99, "sm")]


// We're currently only testing non-uniquely-referenced Arrays of
// refcounted objects to cut down on the excessive number of subtests.
// Revert 3362243cd6f1b6c16dc8d10dd27c8f3e881ecbc7 if you want the full thing
fileprivate func configs() -> [BenchmarkInfo] {
  var configs: [BenchmarkInfo] = []
  for (sourceCount, sourceName) in sourceSizes {
    for (destCount, destName) in destSizes {
      for (subrange, rangeName) in ranges(count: destCount) {
        configs.append(
          BenchmarkInfo(
            name:"ArrayRRC.\(destName).\(sourceName).\(rangeName)",
            runFunction: runArrayRRCRef,
            tags: [.api, .Array, .algorithm],
            setUpFunction: {
              if sourceCount < 1000 && destCount < 1000 {
                extraIters = 100
              }
              if sourceCount > 1000 && destCount > 1000 {
                extraIters = 2
              }
              range = subrange
              refcountedDest = makeRefcounted(count: destCount)
              refcountedSource = makeRefcounted(count: sourceCount)
            },
            tearDownFunction: {
              refcountedDest = []
              refcountedSource = []
              refcountedOriginalRangeContents = []
              extraIters = 10
            }
          )
        )
      }
    }
  }
  return configs
}

fileprivate var range:Range<Int> = 0 ..< 0
fileprivate var refcountedDest:[DummyObject] = []
fileprivate var refcountedSource:[DummyObject] = []
fileprivate var refcountedOriginalRangeContents:[DummyObject] = []
fileprivate var extraIters = 10

public let benchmarks: [BenchmarkInfo] = configs()

@inline(__always)
fileprivate func runArrayRRCImpl<A>(
  n: Int,
  dest destination: A,
  source: A,
  sourceLocation: inout A
) where A:RangeReplaceableCollection, A.Index == Int  {
  let subrange = range
  var dest = destination
  for _ in 0 ..< n {
    for _ in 0 ..< extraIters {
      let destCopy = dest
      let sourceCopy = source
      dest.replaceSubrange(subrange, with: source)
      blackHole(dest)
      blackHole(sourceCopy)
      dest = destCopy
    }
  }
  sourceLocation = source
}

public func runArrayRRCRef(n: Int) {
  let source = refcountedSource
  refcountedSource = []
  runArrayRRCImpl(
    n: n,
    dest: refcountedDest,
    source: source,
    sourceLocation: &refcountedSource
  )
}
