//===--- PrefixWhile.swift ------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
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
let array: [Int] = Array(0..<sequenceCount)

public let benchmarks = [
  BenchmarkInfo(
    name: "PrefixWhileCountableRange",
    runFunction: run_PrefixWhileCountableRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileSequence",
    runFunction: run_PrefixWhileSequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySequence",
    runFunction: run_PrefixWhileAnySequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySeqCntRange",
    runFunction: run_PrefixWhileAnySeqCntRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySeqCRangeIter",
    runFunction: run_PrefixWhileAnySeqCRangeIter,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnyCollection",
    runFunction: run_PrefixWhileAnyCollection,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileArray",
    runFunction: run_PrefixWhileArray,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
  BenchmarkInfo(
    name: "PrefixWhileCountableRangeLazy",
    runFunction: run_PrefixWhileCountableRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileSequenceLazy",
    runFunction: run_PrefixWhileSequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySequenceLazy",
    runFunction: run_PrefixWhileAnySequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySeqCntRangeLazy",
    runFunction: run_PrefixWhileAnySeqCntRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnySeqCRangeIterLazy",
    runFunction: run_PrefixWhileAnySeqCRangeIterLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileAnyCollectionLazy",
    runFunction: run_PrefixWhileAnyCollectionLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "PrefixWhileArrayLazy",
    runFunction: run_PrefixWhileArrayLazy,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
]

@inline(never)
public func run_PrefixWhileCountableRange(_ n: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileSequence(_ n: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySequence(_ n: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCntRange(_ n: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCRangeIter(_ n: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnyCollection(_ n: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileArray(_ n: Int) {
  let s = array
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileCountableRangeLazy(_ n: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileSequenceLazy(_ n: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySequenceLazy(_ n: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCntRangeLazy(_ n: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnySeqCRangeIterLazy(_ n: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileAnyCollectionLazy(_ n: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}
@inline(never)
public func run_PrefixWhileArrayLazy(_ n: Int) {
  let s = (array).lazy
  for _ in 1...20*n {
    var result = 0
    for element in s.prefix(while: {$0 < prefixCount} ) {
      result += element
    }
    check(result == sumCount)
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
