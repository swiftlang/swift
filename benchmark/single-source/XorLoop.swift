//===--- XorLoop.swift ----------------------------------------------------===//
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
public func run_XorLoop(_ N: Int) {
  for _ in 1...5*N {
    let size = 100000
    let ref_result = 47813324
    var x = [Int](repeating: 0xA05FD, count: size)
    for i in 0..<size {
      x[i] = x[i] ^ 12345678
    }
    let res = x[10]+x[100]+x[1000]+x[10000]
    CheckResults(res == ref_result,
                 "Incorrect results in XorLoop: \(res) != \(ref_result)")
  }
}
