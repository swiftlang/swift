//===--- Suffix.swift -----------------------------------------*- swift -*-===//
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
// be directly modified. Instead, make changes to Suffix.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let sequenceCount = 4096
let suffixCount = 1024
let sumCount = suffixCount * (2 * sequenceCount - suffixCount - 1) / 2

public let Suffix = [
  BenchmarkInfo(
    name: "SuffixCountableRange",
    runFunction: run_SuffixCountableRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixSequence",
    runFunction: run_SuffixSequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySequence",
    runFunction: run_SuffixAnySequence,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySeqCntRange",
    runFunction: run_SuffixAnySeqCntRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySeqCRangeIter",
    runFunction: run_SuffixAnySeqCRangeIter,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnyCollection",
    runFunction: run_SuffixAnyCollection,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixArray",
    runFunction: run_SuffixArray,
    tags: [.validation, .api, .Array, .unstable]),
  BenchmarkInfo(
    name: "SuffixCountableRangeLazy",
    runFunction: run_SuffixCountableRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixSequenceLazy",
    runFunction: run_SuffixSequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySequenceLazy",
    runFunction: run_SuffixAnySequenceLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySeqCntRangeLazy",
    runFunction: run_SuffixAnySeqCntRangeLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnySeqCRangeIterLazy",
    runFunction: run_SuffixAnySeqCRangeIterLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixAnyCollectionLazy",
    runFunction: run_SuffixAnyCollectionLazy,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "SuffixArrayLazy",
    runFunction: run_SuffixArrayLazy,
    tags: [.validation, .api, .Array, .unstable]),
]

@inline(never)
public func run_SuffixCountableRange(_ N: Int) {
  let s = 0..<sequenceCount
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixSequence(_ N: Int) {
  let s = sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySequence(_ N: Int) {
  let s = AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySeqCntRange(_ N: Int) {
  let s = AnySequence(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySeqCRangeIter(_ N: Int) {
  let s = AnySequence((0..<sequenceCount).makeIterator())
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnyCollection(_ N: Int) {
  let s = AnyCollection(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixArray(_ N: Int) {
  let s = Array(0..<sequenceCount)
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixCountableRangeLazy(_ N: Int) {
  let s = (0..<sequenceCount).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixSequenceLazy(_ N: Int) {
  let s = (sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil }).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySequenceLazy(_ N: Int) {
  let s = (AnySequence(sequence(first: 0) { $0 < sequenceCount - 1 ? $0 &+ 1 : nil })).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySeqCntRangeLazy(_ N: Int) {
  let s = (AnySequence(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnySeqCRangeIterLazy(_ N: Int) {
  let s = (AnySequence((0..<sequenceCount).makeIterator())).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixAnyCollectionLazy(_ N: Int) {
  let s = (AnyCollection(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}
@inline(never)
public func run_SuffixArrayLazy(_ N: Int) {
  let s = (Array(0..<sequenceCount)).lazy
  for _ in 1...20*N {
    var result = 0
    for element in s.suffix(suffixCount) {
      result += element
    }
    CheckResults(result == sumCount)
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
