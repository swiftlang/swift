//===--- DictionaryCopy.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark checks for quadratic behavior while copying elements in hash
// order between Dictionaries of decreasing capacity
//
// https://bugs.swift.org/browse/SR-3268

import TestsUtils

let t: [BenchmarkCategory] = [.validation, .api, .Dictionary]

// We run the test at a spread of sizes between 1*x and 2*x, because the
// quadratic behavior only happens at certain load factors.

public let DictionaryCopy = [
  BenchmarkInfo(name:"Dict.CopyKeyValue.16k",
    runFunction: copyKeyValue, tags: t, setUpFunction: { dict(16_000) }),
  BenchmarkInfo(name:"Dict.CopyKeyValue.20k",
    runFunction: copyKeyValue, tags: t, setUpFunction: { dict(20_000) }),
  BenchmarkInfo(name:"Dict.CopyKeyValue.24k",
    runFunction: copyKeyValue, tags: t, setUpFunction: { dict(24_000) }),
  BenchmarkInfo(name:"Dict.CopyKeyValue.28k",
    runFunction: copyKeyValue, tags: t, setUpFunction: { dict(28_000) }),

  BenchmarkInfo(name:"Dict.FilterAllMatch.16k",
    runFunction: filterAllMatch, tags: t, setUpFunction: { dict(16_000) }),
  BenchmarkInfo(name:"Dict.FilterAllMatch.20k",
    runFunction: filterAllMatch, tags: t, setUpFunction: { dict(20_000) }),
  BenchmarkInfo(name:"Dict.FilterAllMatch.24k",
    runFunction: filterAllMatch, tags: t, setUpFunction: { dict(24_000) }),
  BenchmarkInfo(name:"Dict.FilterAllMatch.28k",
    runFunction: filterAllMatch, tags: t, setUpFunction: { dict(28_000) }),
]

var dict: [Int: Int]?

func dict(_ size: Int) {
  dict = Dictionary(uniqueKeysWithValues: zip(1...size, 1...size))
}

@inline(never)
func copyKeyValue(N: Int) {
  for _ in 1...N {
    var copy = [Int: Int]()
    for (key, value) in dict! {
      copy[key] = value
    }
    CheckResults(copy.count == dict!.count)
  }
}

// Filter with a predicate returning true is essentially the same loop as the
// one in copyKeyValue above.
@inline(never)
func filterAllMatch(N: Int) {
  for _ in 1...N {
    let copy = dict!.filter { _ in true }
    CheckResults(copy.count == dict!.count)
  }
}
