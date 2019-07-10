//===--- DropFirst.swift --------------------------------------*- swift -*-===//
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
// be directly modified. Instead, make changes to DropFirst.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let sequenceCount = 4096
let dropCount = 1024
let suffixCount = sequenceCount - dropCount
let sumCount = suffixCount * (2 * sequenceCount - suffixCount - 1) / 2
let array: [Int] = Array(0..<sequenceCount)

public let DropFirst = [
  BenchmarkInfo(
    name: "DropFirstCountableRange",
    runFunction: run_DropFirstCountableRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstSequence",
    runFunction: run_DropFirstSequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySequence",
    runFunction: run_DropFirstAnySequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySeqCntRange",
    runFunction: run_DropFirstAnySeqCntRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySeqCRangeIter",
    runFunction: run_DropFirstAnySeqCRangeIter,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnyCollection",
    runFunction: run_DropFirstAnyCollection,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstArray",
    runFunction: run_DropFirstArray,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
  BenchmarkInfo(
    name: "DropFirstCountableRangeLazy",
    runFunction: run_DropFirstCountableRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstSequenceLazy",
    runFunction: run_DropFirstSequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySequenceLazy",
    runFunction: run_DropFirstAnySequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySeqCntRangeLazy",
    runFunction: run_DropFirstAnySeqCntRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnySeqCRangeIterLazy",
    runFunction: run_DropFirstAnySeqCRangeIterLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstAnyCollectionLazy",
    runFunction: run_DropFirstAnyCollectionLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "DropFirstArrayLazy",
    runFunction: run_DropFirstArrayLazy,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(array) }),
]

@inline(never)
public func run_DropFirstCountableRange(_ N: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstSequence(_ N: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySequence(_ N: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySeqCntRange(_ N: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySeqCRangeIter(_ N: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnyCollection(_ N: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstArray(_ N: Int) {
  let s = array
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstCountableRangeLazy(_ N: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstSequenceLazy(_ N: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySequenceLazy(_ N: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySeqCntRangeLazy(_ N: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnySeqCRangeIterLazy(_ N: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstAnyCollectionLazy(_ N: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_DropFirstArrayLazy(_ N: Int) {
  let s = (array).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.dropFirst(dropCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
