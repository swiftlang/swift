//===--- LazyFilter.swift -------------------------------------------------===//
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

// This test checks performance of creating an array from lazily filtered
// collections.
import TestsUtils

@inline(never)
public func run_LazilyFilteredRange(_ N: Int) {
  var res = 123
  let c = (1..<1_000_000).lazy.filter { $0 % 7 == 0 }
  for _ in 1...N {
    res += Array(c).count
    res -= Array(c).count
  }
  CheckResults(res == 123, "Wrong result in LazilyFilteredRange 123 != \(res)")
}

@inline(never)
public func run_LazilyFilteredArrays(_ N: Int) {
  var res = 123
  let c = (1..<1_000_000).map({[$0]}).lazy.filter { $0.first! % 7 == 0 }
  for _ in 1...N {
    res += Array(c).count
    res -= Array(c).count
  }
  CheckResults(res == 123, "Wrong result in LazilyFilteredArray 123 != \(res)")
}

