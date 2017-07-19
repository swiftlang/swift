//===--- PrefixWhile.swift ------------------------------------*- swift -*-===//
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
// be directly modified. Instead, make changes to PrefixWhile.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let sequenceCount = 4096
let prefixCount = sequenceCount - 1024
let sumCount = prefixCount * (prefixCount - 1) / 2

@inline(never)
public func run_PrefixWhileCountableRange(_ N: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileSequence(_ N: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySequence(_ N: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCntRange(_ N: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCRangeIter(_ N: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnyCollection(_ N: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileArray(_ N: Int) {
  let s = Array(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileCountableRangeLazy(_ N: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileSequenceLazy(_ N: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySequenceLazy(_ N: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCntRangeLazy(_ N: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCRangeIterLazy(_ N: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnyCollectionLazy(_ N: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileArrayLazy(_ N: Int) {
  let s = (Array(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
