//===--- DropLast.swift ---------------------------------------*- swift -*-===//
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
// be directly modified. Instead, make changes to DropLast.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let sequenceCount = 4096
let prefixCount = 1024
let dropCount = sequenceCount - prefixCount
let sumCount = prefixCount * (prefixCount - 1) / 2
let array: [Int] = Array(0..<sequenceCount)

public let DropLast = [
  BenchmarkInfo(
    name: "DropLastCountableRange",
    runFunction: run_DropLastCountableRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastSequence",
    runFunction: run_DropLastSequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySequence",
    runFunction: run_DropLastAnySequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySeqCntRange",
    runFunction: run_DropLastAnySeqCntRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySeqCRangeIter",
    runFunction: run_DropLastAnySeqCRangeIter,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnyCollection",
    runFunction: run_DropLastAnyCollection,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastArray",
    runFunction: run_DropLastArray,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
  BenchmarkInfo(
    name: "DropLastCountableRangeLazy",
    runFunction: run_DropLastCountableRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastSequenceLazy",
    runFunction: run_DropLastSequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySequenceLazy",
    runFunction: run_DropLastAnySequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySeqCntRangeLazy",
    runFunction: run_DropLastAnySeqCntRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnySeqCRangeIterLazy",
    runFunction: run_DropLastAnySeqCRangeIterLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastAnyCollectionLazy",
    runFunction: run_DropLastAnyCollectionLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropLastArrayLazy",
    runFunction: run_DropLastArrayLazy,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
]

@inline(never)
public func run_DropLastCountableRange(_ N: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastSequence(_ N: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySequence(_ N: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySeqCntRange(_ N: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySeqCRangeIter(_ N: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnyCollection(_ N: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastArray(_ N: Int) {
  let s = array
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastCountableRangeLazy(_ N: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastSequenceLazy(_ N: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySequenceLazy(_ N: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySeqCntRangeLazy(_ N: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnySeqCRangeIterLazy(_ N: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastAnyCollectionLazy(_ N: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropLastArrayLazy(_ N: Int) {
  let s = (array).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropLast(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
