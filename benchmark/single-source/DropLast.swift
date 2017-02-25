//===--- DropLast.swift ---------------------------------------------------===//
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
let prefixCount = 1024
let dropCount = sequenceCount - prefixCount
let sumCount = prefixCount * (prefixCount-1) / 2

@inline(never)
public func run_DropLastCountableRange(_ N: Int) {
  let s = 0 ..< sequenceCount
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.dropLast(dropCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in DropLastCountableRange: \(result) != \(sumCount)")
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
public func run_DropLastSequence(_ N: Int) {
  let s = MySequence(range: 0 ..< sequenceCount)
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.dropLast(dropCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in DropLastSequence: \(result) != \(sumCount)")
    }
  }
}

@inline(never)
public func run_DropLastAnySequence(_ N: Int) {
  let s = AnySequence(0 ..< sequenceCount)
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.dropLast(dropCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in DropLastAnySequence: \(result) != \(sumCount)")
    }
  }
}

@inline(never)
public func run_DropLastArray(_ N: Int) {
  let s = Array(0 ..< sequenceCount)
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      for element in s.dropLast(dropCount) {
        result += element
      }
      CheckResults(result == sumCount, 
        "IncorrectResults in DropLastArray: \(result) != \(sumCount)")
    }
  }
}
