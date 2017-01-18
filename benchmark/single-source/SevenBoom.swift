//===--- SevenBoom.swift --------------------------------------------------===//
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

@inline(never)
func filter_seven(_ input : Int) throws {
  guard case 7 = input else {
    throw NSError(domain: "AnDomain", code: 42, userInfo: nil)
  }
}

@inline(never)
public func run_SevenBoom(_ N: Int) {
  var c = 0
  for i in 1...N*5000 {
    do {
      try filter_seven(i)
      c += 1
    }
    catch _ {
    }
  }
  CheckResults(c == 1, "IncorrectResults in SevenBoom")
}

