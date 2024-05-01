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

fileprivate class DummyObject {}

fileprivate func makePOD(count: Int) -> [Int] {
  Array(repeating: 42, count: count)
}

fileprivate func makeRefcounted(count: Int) -> [DummyObject] {
  (0 ..< count).map { _ in DummyObject() }
}

fileprivate func ranges(count: Int) -> [(Range<Int>, String)] {
  var results:[(Range<Int>, String)] = []
  results.append((0 ..< (count / 3), "front"))
  results.append(((count / 3) ..< ((count / 3) * 2), "middle"))
  results.append((((count / 3) * 2) ..< count, "end"))
  return results
}

fileprivate let sizes = [(100_000, "large"), (100, "small"), (3, "tiny")]

fileprivate func configs() -> [BenchmarkInfo] {
  var configs: [BenchmarkInfo] = []
  for (refcounted, refcountedName) in [(true, "refcounted"), (false, "pod")] {
    for (sourceCount, sourceName) in sizes {
      for (destCount, destName) in sizes {
        for (subrange, rangeName) in ranges(count: destCount) {
          for (sourceUnique, sourceUniqueName) in [(true, "sourceUnique"), (false, "sourceNonUnique")] {
            for (destUnique, destUniqueName) in [(true, "destUnique"), (false, "destNonUnique")] {
              let runFunction = switch (refcounted, destUnique) {
              case (true, true): runArrayRRCRefcountedUniqueDest
              case (true, false): runArrayRRCRefcountedSharedDest
              case (false, true): runArrayRRCPODUniqueDest
              case (false, false): runArrayRRCPODSharedDest
              }
              configs.append(
                BenchmarkInfo(
                  name:"arrayRRC_\(refcountedName)_\(destName)_\(sourceName)_\(rangeName)_\(destUniqueName)_\(sourceUniqueName)",
                  runFunction: runFunction,
                  tags: [.api, .Array, .algorithm],
                  setUpFunction: {
                    useUniqueDest = destUnique
                    useUniqueSource = sourceUnique
                    if refcounted {
                      refcountedDest = makeRefcounted(count: destCount)
                      refcountedSource = makeRefcounted(count: sourceCount)
                      if useUniqueDest {
                        refcountedOriginalRangeContents = Array(refcountedDest[range])
                      }
                    } else {
                      podDest = makePOD(count: destCount)
                      podSource = makePOD(count: sourceCount)
                      if useUniqueDest {
                        podOriginalRangeContents = Array(podDest[range])
                      }
                    }
                    range = subrange
                  },
                  tearDownFunction: {
                    refcountedDest = []
                    refcountedSource = []
                    refcountedOriginalRangeContents = []
                    podDest = []
                    podSource = []
                    podOriginalRangeContents = []
                  }
                )
              )
            }
          }
        }
      }
    }
  }
  return configs
}

fileprivate var range:Range<Int> = 0 ..< 0
fileprivate var refcountedDest:[DummyObject] = []
fileprivate var podDest:[Int] = []
fileprivate var refcountedSource:[DummyObject] = []
fileprivate var podSource:[Int] = []
fileprivate var refcountedOriginalRangeContents:[DummyObject] = []
fileprivate var podOriginalRangeContents:[Int] = []
fileprivate var useUniqueDest = false
fileprivate var useUniqueSource = false

public let benchmarks: [BenchmarkInfo] = configs()

/*
 Note: the work done by the unique and non-unique variants is different.
 Only compare like to like.
 */
fileprivate func runArrayRRCImplNonUniqueDest<A>(
  n: Int,
  dest: consuming A,
  source: consuming A
) where A:RangeReplaceableCollection, A.Index == Int  {
  let subrange = range
  for _ in 0 ..< n {
    let destCopy = dest
    let sourceCopy = useUniqueSource ? nil : source
    dest.replaceSubrange(subrange, with: source)
    blackHole(dest)
    if useUniqueSource {
      blackHole(sourceCopy)
    }
    dest = destCopy
  }
}

/*
 Keeping the destination unique requires restoring it to its original state each
 iteration, which necessarily involves a different replaceSubrange than the one
 being tested. Unfortunately this makes this variant less precise than the
 other, but it's still useful to be able to see the non-CoW case
 */
fileprivate func runArrayRRCImplUniqueDest<A>(
  n: Int,
  dest: consuming A,
  source: consuming A,
  originalRangeContents: consuming A
) where A:RangeReplaceableCollection, A.Index == Int {
  let subrange = range
  for _ in 0 ..< n {
    let sourceCopy = useUniqueSource ? nil : source
    let originalRangeContentsCopy = useUniqueSource ? nil : originalRangeContents
    dest.replaceSubrange(subrange, with: source)
    blackHole(dest)
    dest.replaceSubrange(subrange, with: originalRangeContents)
    if useUniqueSource {
      blackHole(originalRangeContentsCopy)
      blackHole(sourceCopy)
    }
  }
}

public func runArrayRRCRefcountedUniqueDest(n: Int) {
  let source = refcountedSource
  refcountedSource = []
  let dest = refcountedDest
  refcountedDest = []
  let originalRangeContents = refcountedOriginalRangeContents
  refcountedOriginalRangeContents = []
  runArrayRRCImplUniqueDest(
    n: n,
    dest: dest,
    source: source,
    originalRangeContents: originalRangeContents
  )
}

public func runArrayRRCRefcountedSharedDest(n: Int) {
  let source = refcountedSource
  refcountedSource = []
  runArrayRRCImplNonUniqueDest(
    n: n,
    dest: refcountedDest,
    source: source
  )
}

public func runArrayRRCPODUniqueDest(n: Int) {
  let source = podSource
  podSource = []
  let dest = podDest
  podDest = []
  let originalRangeContents = podOriginalRangeContents
  podOriginalRangeContents = []
  runArrayRRCImplUniqueDest(
    n: n,
    dest: dest,
    source: source,
    originalRangeContents: originalRangeContents
  )
}

public func runArrayRRCPODSharedDest(n: Int) {
  let source = podSource
  podSource = []
  runArrayRRCImplNonUniqueDest(
    n: n,
    dest: podDest,
    source: source
  )
}
