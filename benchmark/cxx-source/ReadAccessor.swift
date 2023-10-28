// Subscripts.swift - Very brief description
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// This is a simple test that reads a non trivial C++ struct using an imported
/// subscript thousands of times.
///
// -----------------------------------------------------------------------------

#if FIXED117438849

import TestsUtils
import CxxSubscripts

var vec : TwoDimensionalVector?

public let benchmarks = [
  BenchmarkInfo(
    name: "ReadAccessor",
    runFunction: run_ReadAccessor,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: {
        vec = initVector()
    })
]

@inline(never)
public func run_ReadAccessor(_ N: Int) {
  for _ in 0...N {
    for j in 0..<100 {
      let row = vec![j];
      for k in 0..<1_000 {
        let element = row[k];
        blackHole(element)
      }
    }
  }
}

#endif
