//===--- Suffix.swift ---------------------------------------------------===//
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
let sequenceCount = 4096
let suffixCount = 1024
let sumCount = suffixCount * (2 * sequenceCount - suffixCount - 1) / 2

@inline(never)
public func run_SuffixCountableRange(_ N: Int) {
  let s = 0 ..< sequenceCount
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.suffix(suffixCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in SuffixCountableRange: \(result) != \(sumCount)")
    }
  }
}

fileprivate struct MySequence: Sequence {
  let range: CountableRange<Int>
  public func makeIterator() -> IndexingIterator<CountableRange<Int>> {
      return range.makeIterator()
  }
}

@inline(never)
public func run_SuffixSequence(_ N: Int) {
  let s = MySequence(range: 0 ..< sequenceCount)
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.suffix(suffixCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in SuffixSequence: \(result) != \(sumCount)")
    }
  }
}

@inline(never)
public func run_SuffixAnySequence(_ N: Int) {
  let s = AnySequence(0 ..< sequenceCount)
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.suffix(suffixCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in SuffixAnySequence: \(result) != \(sumCount)")
    }
  }
}

