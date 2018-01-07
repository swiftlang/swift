//===--- PointerArithmetics.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let PointerArithmetics = [
  BenchmarkInfo(name: "PointerPlusInt", runFunction: run_PointerPlusInt, tags: [.validation, .api]),
]

@inline(never)
public func run_PointerPlusInt(_ N: Int) {
  var numbers = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  var c = 0
  withUnsafePointer(to: &numbers) {
    $0.withMemoryRebound(to: Int.self, capacity: 10) { ptr in
      for i in 1...N*10_000_000 {
        c += (ptr + i%10).pointee
      }
    }
  }
  CheckResults(c != 0)
}
