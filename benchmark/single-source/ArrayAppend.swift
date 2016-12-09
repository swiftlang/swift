//===--- ArrayAppend.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test checks the performance of appending to an array.

import TestsUtils

// Append single element
@inline(never)
public func run_ArrayAppend(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}

// Append single element with reserve
@inline(never)
public func run_ArrayAppendReserved(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       nums.reserveCapacity(40000)
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}

// Append a sequence. Length of sequence unknown so
// can't pre-reserve capacity. 
@inline(never)
public func run_ArrayAppendSequence(_ N: Int) {
  let seq = stride(from: 0, to: 10_000, by: 1)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums.append(contentsOf: seq)
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendArrayOfInt(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendStrings(_ N: Int) {
  let other = stride(from: 0, to: 10_000, by: 1).map { "\($0)" }
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [String]()
      // lower inner count due to string slowness
      for _ in 0..<4 {
        nums += other
      }
    }
  }
}

struct S<T,U> {
  var x: T
  var y: U
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendGenericStructs(_ N: Int) {
  let other = Array(repeating: S(x: 3, y: 4.2), count: 10_000)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [S<Int,Double>]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendOptionals(_ N: Int) {
  let other: [Int?] = Array(repeating: 1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int?]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


// Append a lazily-mapped array. Length of sequence known so
// can pre-reserve capacity, but no optimization points used.
@inline(never)
public func run_ArrayAppendLazyMap(_ N: Int) {
  let other = Array(0..<10_000).lazy.map { $0 * 2 }

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


// Append a Repeat collection. Length of sequence known so
// can pre-reserve capacity, but no optimization points used.
@inline(never)
public func run_ArrayAppendRepeatCol(_ N: Int) {
  let other = repeatElement(1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


