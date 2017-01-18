//===--- Array2D.swift ----------------------------------------------------===//
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

@inline(never)
public func run_Array2D(_ N: Int) {
  var A: [[Int]] = []
  for _ in 0 ..< 1024 {
    var B: [Int] = []
    for y in 0 ..< 1024 {
      B.append(y)
    }
    A.append(B)
  }
  for _ in 0..<N {
    for i in 0 ..< 1024 {
      for y in 0 ..< 1024 {
        A[i][y] = A[i][y] + 1
        A[i][y] = A[i][y] - 1
      }
    }
  }
}
