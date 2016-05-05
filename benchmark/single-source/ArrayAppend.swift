//===--- ArrayAppend.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test checks the performance of appending to an array.

import TestsUtils

@inline(never)
public func run_ArrayAppend(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}

@inline(never)
public func run_ArrayAppendReserved(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       nums.reserveCapacity(40000)
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}
