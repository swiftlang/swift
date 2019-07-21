//===--- CaptureProp.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let CaptureProp = BenchmarkInfo(
  name: "CaptureProp",
  runFunction: run_CaptureProp,
  tags: [.validation, .api, .refcount],
  legacyFactor: 10)

func sum(_ x:Int, y:Int) -> Int {
  return x + y
}

@inline(never)
func benchCaptureProp<S : Sequence
>(
  _ s: S, _ f: (S.Element, S.Element) -> S.Element) -> S.Element {

  var it = s.makeIterator()
  let initial = it.next()!
  return IteratorSequence(it).reduce(initial, f)
}

public func run_CaptureProp(_ N: Int) {
  let a = 1...10_000
  for _ in 1...10*N {
    _ = benchCaptureProp(a, sum)
  }
}
