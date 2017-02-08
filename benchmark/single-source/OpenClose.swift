//===--- OpenClose.swift --------------------------------------------------===//
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


// A micro benchmark for checking the speed of string-based enums.

enum MyState : String {
    case Closed = "Closed"
    case Opened = "Opened"
}

@inline(never)
func check_state(_ state : MyState) -> Int {
  return state == .Opened ? 1 : 0
}

@inline(never)
public func run_OpenClose(_ N: Int) {
  var c = 0
  for _ in 1...N*10000 {
      c += check_state(MyState.Closed)
  }
  CheckResults(c == 0, "IncorrectResults in run_OpenClose")
}

