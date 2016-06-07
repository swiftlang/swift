//===--- MapReduce.swift --------------------------------------------------===//
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

import TestsUtils
import Foundation

@inline(never)
public func run_MapReduce(_ N: Int) {
  var numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*100 {
    numbers = numbers.map({$0 &+ 5})
    c += numbers.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

