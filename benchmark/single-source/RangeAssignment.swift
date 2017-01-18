//===--- RangeAssignment.swift --------------------------------------------===//
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

@inline(never)
public func run_RangeAssignment(_ scale: Int) {
  let range: Range = 100..<200
  var vector = [Double](repeating: 0.0 , count: 5000)
  let alfa = 1.0
  let N = 500*scale
  for _ in 1...N {
      vector[range] = ArraySlice(vector[range].map { $0 + alfa })
  }

  CheckResults(vector[100] == Double(N),
    "IncorrectResults in RangeAssignment: \(vector[100]) != \(N).")
}
