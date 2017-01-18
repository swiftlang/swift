//===--- BitCount.swift ---------------------------------------------------===//
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

// This test checks performance of Swift bit count.
// and mask operator.
// rdar://problem/22151678
import Foundation
import TestsUtils

func countBitSet(_ num: Int) -> Int {
  let bits = MemoryLayout<Int>.size * 8
  var cnt: Int = 0
  var mask: Int = 1
  for _ in 0...bits {
    if num & mask != 0 {
      cnt += 1
    }
    mask <<= 1
  }
  return cnt
}

@inline(never)
public func run_BitCount(_ N: Int) {
  for _ in 1...100*N {
    // Check some results.
    CheckResults(countBitSet(1) == 1, "Incorrect results in BitCount.")
    CheckResults(countBitSet(2) == 1, "Incorrect results in BitCount.")
    CheckResults(countBitSet(2457) == 6, "Incorrect results in BitCount.")
  }
}
