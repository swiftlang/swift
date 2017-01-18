//===--- GlobalClass.swift ------------------------------------------------===//
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

// Test inline cache with a global class. Make sure the retain|release pair
// for the fast path is removed in the loop.
import TestsUtils

class A
{
  func f(_ a: Int) -> Int
  {
    return a + 1
  }
}

var x = 0
var a = A()
@inline(never)
public func run_GlobalClass(_ N: Int) {
  for _ in 0..<N
  {
    x = a.f(x)
  }
}
