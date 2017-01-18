//===--- PopFront.swift ---------------------------------------------------===//
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

let reps = 1
let arrayCount = 1024

@inline(never)
public func run_PopFrontArray(_ N: Int) {
  let orig = Array(repeating: 1, count: arrayCount)
  var a = [Int]()
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      a.append(contentsOf: orig)
      while a.count != 0 {
        result += a[0]
        a.remove(at: 0)
      }
      CheckResults(result == arrayCount, "IncorrectResults in StringInterpolation: \(result) != \(arrayCount)")
    }
  }
}

@inline(never)
public func run_PopFrontUnsafePointer(_ N: Int) {
  var orig = Array(repeating: 1, count: arrayCount)
  let a = UnsafeMutablePointer<Int>.allocate(capacity: arrayCount)
  for _ in 1...100*N {
    for _ in 1...reps {
      for i in 0..<arrayCount {
        a[i] = orig[i]
      }
      var result = 0
      var count = arrayCount
      while count != 0 {
        result += a[0]
        a.assign(from: a + 1, count: count - 1)
        count -= 1
      }
      CheckResults(result == arrayCount, "IncorrectResults in StringInterpolation: \(result) != \(arrayCount)")
    }
  }
  a.deallocate(capacity: arrayCount)
}

