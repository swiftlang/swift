//===--- ProtocolDispatch.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let ProtocolConformance = BenchmarkInfo (
  name: "ProtocolConformance",
  runFunction: run_ProtocolConformance,
  tags: [.validation, .runtime])

protocol P {}

struct One: P {}
struct Two {}

struct Cat<T, U> {}

extension Cat: P where T: P, U: P {}

protocol Growable {}
extension Growable {
  func grow() -> (Growable, Growable) {
    return (Cat<Self, One>(), Cat<Self, Two>())
  }
}

extension One: Growable {}
extension Two: Growable {}
extension Cat: Growable {}

@inline(never)
public func run_ProtocolConformance(_ N: Int) {
  var array: [Growable] = [One(), Two()]
  var i = 0
  var checks = 0

  // The expected number of times we expect `elt is P` to be true.
  var expectedConforms = 0

  // The expected number of times we expect `elt is P` to be true
  // per iteration, at the current time.
  var expectedConformsPerIter = 1

  // The number of times we've actually seen `elt is P` be true.
  var conforms = 0
  while checks < N * 500 {
    let (a, b) = array[i].grow()
    array.append(a)
    array.append(b)

    // The number of times `elt is P` is true per outer iteration
    // goes up by 1 when the array's count is a power of 2.
    if array.count & (array.count - 1) == 0 {
      expectedConformsPerIter += 1
    }
    expectedConforms += expectedConformsPerIter

    for elt in array {
      if elt is P {
        conforms += 1
      }
      checks += 1
    }
    i += 1
  }
  CheckResults(expectedConforms == conforms)
}
