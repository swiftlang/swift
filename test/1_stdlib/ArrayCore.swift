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

//===--- class Tracked ----------------------------------------------------===//
// Instead of testing with Int elements, we use this wrapper class
// that can help us track allocations and find issues with object
// lifetime inside Array implementations.
var trackedCount = 0
var nextTrackedSerialNumber = 0

final class Tracked : ForwardIndexType, CustomStringConvertible {
  required init(_ value: Int) {
    trackedCount += 1
    nextTrackedSerialNumber += 1
    serialNumber = nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    trackedCount -= 1
    serialNumber = -serialNumber
  }

  var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }

  func successor() -> Self {
    return self.dynamicType.init(self.value.successor())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

//===--- struct MrMcRange -------------------------------------------------===//
// A wrapper around Range<Tracked> that allows us to detect when it is
// being treated as a CollectionType rather than merely a SequenceType, which
// helps us to prove that an optimization is being used.  In
// particular, when constructing a _ContiguousArrayBuffer from a
// CollectionType, the necessary storage should be pre-allocated.
struct MrMcRange : CollectionType {
  typealias Base = Range<Int>

  init(_ base: Base) {
    self.base = base
  }

  func generate() -> IndexingGenerator<MrMcRange> {
    return IndexingGenerator(self)
  }
  
  var startIndex: Int {
    print("using collection API")
    return base.startIndex
  }
  
  var endIndex: Int {
    return base.endIndex
  }

  subscript(i: Int) -> Tracked {
    return Tracked(i)
  }
  
  var base: Base
}

func printSequence<T : SequenceType>(x: T) {
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

  let n0 = ((Tracked(10)..<Tracked(27)).generate())._copyToNativeArrayBuffer()
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
print("trackedCount = \(trackedCount)")

// CHECK-NEXT: done.
print("done.")
