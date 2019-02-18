//===--- Calculator.swift -------------------------------------------------===//
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
import Foundation

public let Calculator = BenchmarkInfo(
  name: "Calculator",
  runFunction: run_Calculator,
  tags: [.validation])

@inline(never)
func my_atoi_impl(_ input : String) -> Int {
  switch input {
    case "0": return 0
    case "1": return 1
    case "2": return 2
    case "3": return 3
    case "4": return 4
    case "5": return 5
    case "6": return 6
    case "7": return 7
    case "8": return 8
    case "9": return 9
    default: return 0
  }
}

@inline(never)
public func run_Calculator(_ N: Int) {
  var c = 0
  for _ in 1...N*800 {
      c += my_atoi_impl(identity("1"))
      c += my_atoi_impl(identity("2"))
      c += my_atoi_impl(identity("3"))
      c += my_atoi_impl(identity("4"))
      c += my_atoi_impl(identity("5"))
      c += my_atoi_impl(identity("6"))
      c += my_atoi_impl(identity("7"))
      c += my_atoi_impl(identity("8"))
      c += my_atoi_impl(identity("9"))
      c += my_atoi_impl(identity("10"))
      c -= 45
  }
  CheckResults(c == 0)
}

