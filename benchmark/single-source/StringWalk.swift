//===--- StringWalk.swift -------------------------------------------------===//
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

// Test String subscript performance.
//
// Subscript has a slow path that initializes a global variable:
// Swift._cocoaStringSubscript.addressor.  Global optimization would
// normally hoist the initializer outside the inner loop (over
// unicodeScalars), forcing the initializer to be called on each
// lap. However, no that the cocoa code is properly marked "slowPath",
// no hoisting should occur.
import TestsUtils

var count: Int = 0

@inline(never) func countChars(_ s: String) {
  for _ in s.unicodeScalars {
    count += 1
  }
}

@inline(never)
public func run_StringWalk(_ N: Int) {
  let s = "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"

  for _ in 1...50000*N {
    countChars(s)
  }
}
