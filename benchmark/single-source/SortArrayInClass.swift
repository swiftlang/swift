//===--- SortArrayInClass.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// This benchmark is derived from user code that encoutered a major
// performance problem in normal usage. Contributed by Saleem
// Abdulrasool (compnerd).
//
//===----------------------------------------------------------------------===//

import TestsUtils

// Specifically tests efficient access to Array subscript when the
// array is a class property, but also generally tests quicksort in a
// class which needs a slew of array optimizations, uniqueness, bounds
// and exclusivity optimizations.
public let SortArrayInClass = [
  BenchmarkInfo(
    name: "SortArrayInClass",
    runFunction: run_SortArrayInClass,
    tags: [.abstraction, .safetychecks, .exclusivity, .algorithm, .api, .Array])
]

let LARGE_ARRAY_SIZE = 10000

class Sorter {
  var array: [Int]
  init(size: Int) {
    array = Array((0..<size).reversed())
  }

  private func _swap(i: Int, j: Int) {
    let t = array[i]
    // This currently copies the entire array.  Assigning to a
    // temporary, or using swapAt would avoid the copy, but users
    // shouldn't need to know that.
    array[i] = array[j]
    array[j] = t
  }

  private func _quicksort(left: Int, right: Int) {

    if left < right {
      let pivot = array[left + ((right - left) / 2)]
      var left_new = left
      var right_new = right

      repeat {
        while array[left_new] < pivot {
          left_new += 1
        }
        while pivot < array[right_new] {
          right_new -= 1
        }
        if left_new <= right_new {
          _swap(i:left_new, j:right_new)
          left_new += 1
          right_new -= 1
        }
      } while left_new <= right_new

      _quicksort(left: left, right: right_new)
      _quicksort(left: left_new, right:right)
    }
  }

  func quicksort() {
    _quicksort(left:0, right:array.count - 1);
  }
}

public func run_SortArrayInClass(_ N: Int) {
  for _ in 1...N {
    // The array needs to be reinitialized before each sort, so it
    // can't be a setup/tearDown function.
    let sorter = Sorter(size:LARGE_ARRAY_SIZE)
    sorter.quicksort()
  }
}
