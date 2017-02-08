//===--- Sim2DArray.swift -------------------------------------------------===//
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

struct Array2D {
  var storage : [Int]
  let rows : Int
  let cols: Int

  init(numRows: Int, numCols: Int) {
    storage = [Int](repeating: 0, count: numRows * numCols)
    rows = numRows
    cols = numCols
  }
}

@inline(never)
func workload_2DArrayTest(_ A: inout Array2D) {
  for _ in 0 ..< 10 {
    for r in 0 ..< A.rows {
      for c in 0 ..< A.cols {
        A.storage[r*A.cols+c] = 1
      }
    }
  }
}

@inline(never)
public func run_Sim2DArray(_ N: Int) {
  for _ in 0 ..< N {
    var A = Array2D(numRows:2048, numCols:32)
    workload_2DArrayTest(&A)
  }
}
