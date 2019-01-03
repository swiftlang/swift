//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to Zip.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

fileprivate let collectionCount = 4096
fileprivate let sumCount = collectionCount * (collectionCount - 1) / 2

public let Zip = [
  BenchmarkInfo(name: "Zip2Iteration", runFunction: run_Zip2Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "ZipLongest2Iteration", runFunction: run_ZipLongest2Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "Zip3Iteration", runFunction: run_Zip3Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "ZipLongest3Iteration", runFunction: run_ZipLongest3Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "Zip4Iteration", runFunction: run_Zip4Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "ZipLongest4Iteration", runFunction: run_ZipLongest4Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "Zip5Iteration", runFunction: run_Zip5Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "ZipLongest5Iteration", runFunction: run_ZipLongest5Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "Zip6Iteration", runFunction: run_Zip6Iteration, tags: [.validation, .api]),
  BenchmarkInfo(name: "ZipLongest6Iteration", runFunction: run_ZipLongest6Iteration, tags: [.validation, .api])
]

@inline(never)
public func run_Zip2Iteration (n: Int) {
  let collection = zip(
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0)

    for element in collection {
      sums.0 += element.0
      sums.1 += element.1
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
  }
}

@inline(never)
public func run_ZipLongest2Iteration (n: Int) {
  let collection = zipLongest(
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0)

    for element in collection {
      sums.0 += element.0!
      sums.1 += element.1!
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
  }
}

@inline(never)
public func run_Zip3Iteration (n: Int) {
  let collection = zip(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0)

    for element in collection {
      sums.0 += element.0
      sums.1 += element.1
      sums.2 += element.2
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
  }
}

@inline(never)
public func run_ZipLongest3Iteration (n: Int) {
  let collection = zipLongest(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0)

    for element in collection {
      sums.0 += element.0!
      sums.1 += element.1!
      sums.2 += element.2!
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
  }
}

@inline(never)
public func run_Zip4Iteration (n: Int) {
  let collection = zip(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0
      sums.1 += element.1
      sums.2 += element.2
      sums.3 += element.3
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
  }
}

@inline(never)
public func run_ZipLongest4Iteration (n: Int) {
  let collection = zipLongest(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0!
      sums.1 += element.1!
      sums.2 += element.2!
      sums.3 += element.3!
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
  }
}

@inline(never)
public func run_Zip5Iteration (n: Int) {
  let collection = zip(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0
      sums.1 += element.1
      sums.2 += element.2
      sums.3 += element.3
      sums.4 += element.4
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
    CheckResults(sums.4 == sumCount)
  }
}

@inline(never)
public func run_ZipLongest5Iteration (n: Int) {
  let collection = zipLongest(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0!
      sums.1 += element.1!
      sums.2 += element.2!
      sums.3 += element.3!
      sums.4 += element.4!
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
    CheckResults(sums.4 == sumCount)
  }
}

@inline(never)
public func run_Zip6Iteration (n: Int) {
  let collection = zip(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0
      sums.1 += element.1
      sums.2 += element.2
      sums.3 += element.3
      sums.4 += element.4
      sums.5 += element.5
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
    CheckResults(sums.4 == sumCount)
    CheckResults(sums.5 == sumCount)
  }
}

@inline(never)
public func run_ZipLongest6Iteration (n: Int) {
  let collection = zipLongest(
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount,
    0..<collectionCount
  )

  for _ in 1...n {
    var sums = (0, 0, 0, 0, 0, 0)

    for element in collection {
      sums.0 += element.0!
      sums.1 += element.1!
      sums.2 += element.2!
      sums.3 += element.3!
      sums.4 += element.4!
      sums.5 += element.5!
    }

    CheckResults(sums.0 == sumCount)
    CheckResults(sums.1 == sumCount)
    CheckResults(sums.2 == sumCount)
    CheckResults(sums.3 == sumCount)
    CheckResults(sums.4 == sumCount)
    CheckResults(sums.5 == sumCount)
  }
}
