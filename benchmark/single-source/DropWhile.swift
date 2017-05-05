//===--- DropWhile.swift --------------------------------------*- swift -*-===//
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

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to DropWhile.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let sequenceCount = 4096
let dropCount = 1024
let suffixCount = sequenceCount - dropCount
let sumCount = suffixCount * (2 * sequenceCount - suffixCount - 1) / 2

@inline(never)
public func run_DropWhileCountableRange(_ N: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileSequence(_ N: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySequence(_ N: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySeqCntRange(_ N: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySeqCRangeIter(_ N: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnyCollection(_ N: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileArray(_ N: Int) {
  let s = Array(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileCountableRangeLazy(_ N: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileSequenceLazy(_ N: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySequenceLazy(_ N: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySeqCntRangeLazy(_ N: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnySeqCRangeIterLazy(_ N: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileAnyCollectionLazy(_ N: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropWhileArrayLazy(_ N: Int) {
  let s = (Array(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.drop(while: {$0 < dropCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
