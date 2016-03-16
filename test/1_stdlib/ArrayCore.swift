//===--- ArrayCore.swift --------------------------------------------------===//
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
// RUN: %target-run-stdlib-swift | FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest

//===--- struct MrMcRange -------------------------------------------------===//
// A wrapper around Range<LifetimeTracked> that allows us to detect when
// it is being treated as a Collection rather than merely a Sequence, which
// helps us to prove that an optimization is being used.  In
// particular, when constructing a _ContiguousArrayBuffer from a
// Collection, the necessary storage should be pre-allocated.
struct MrMcRange : Collection {
  typealias Base = Range<Int>

  init(_ base: Base) {
    self.base = base
  }

  var startIndex: Int {
    print("using collection API")
    return base.lowerBound
  }
  
  var endIndex: Int {
    return base.upperBound
  }

  subscript(i: Int) -> LifetimeTracked {
    return LifetimeTracked(i)
  }
  
  var base: Base
}

func printSequence<T : Sequence>(x: T) {
  print("<", terminator: "")
  var prefix = ""
  for a in x {
    print(prefix, terminator: "")
    print(a, terminator: "")
    prefix = " "
  }
  print(">")
}

// CHECK: testing...
print("testing...")

func test() {
  //===--- Sequences can be converted -------------------------------------===//

  let n0 = (
    LifetimeTracked(10)..<LifetimeTracked(27)
  ).makeIterator()._copyToNativeArrayBuffer()
  // CHECK-NEXT: <10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26>
  printSequence(n0)

  //===--- Collections get measured ---------------------------------------===//

  // CHECK-NEXT: using collection API
  let n1 = MrMcRange(3..<23)._copyToNativeArrayBuffer()
  // CHECK: <3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22>
  printSequence(n1)
}
test()

// CHECK-NEXT: trackedCount = 0
print("trackedCount = \(LifetimeTracked.instances)")

// CHECK-NEXT: done.
print("done.")
