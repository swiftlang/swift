//===--- DictionaryRemove.swift -------------------------------------------===//
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

// Dictionary element removal benchmark
// rdar://problem/19804127
import TestsUtils

let t: [BenchmarkCategory] = [.validation, .api, .Dictionary]

let size = 100
let numberMap = Dictionary(uniqueKeysWithValues: zip(1...size, 1...size))
let boxedNums = (1...size).lazy.map { Box($0) }
let boxedNumMap = Dictionary(uniqueKeysWithValues: zip(boxedNums, boxedNums))

public let benchmarks = [
  BenchmarkInfo(
    name: "DictionaryRemove",
    runFunction: remove,
    tags: t,
    setUpFunction: { blackHole(numberMap) },
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DictionaryRemoveOfObjects",
    runFunction: removeObjects,
    tags: t,
    setUpFunction: { blackHole(boxedNumMap) },
    legacyFactor: 100),

  BenchmarkInfo(
    name: "DictionaryFilter",
    runFunction: filter,
    tags: t,
    setUpFunction: { blackHole(numberMap) },
    legacyFactor: 1),

  BenchmarkInfo(
    name: "DictionaryFilterOfObjects",
    runFunction: filterObjects,
    tags: t,
    setUpFunction: { blackHole(boxedNumMap) },
    legacyFactor: 1),
]

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

func remove(n: Int) {
  for _ in 1...100*n {
    var dict = numberMap
    for i in 1...size { dict.removeValue(forKey: i) }
    check(dict.isEmpty)
  }
}

func removeObjects(n: Int) {
  for _ in 1...10*n {
    var dict = boxedNumMap
    for i in 1...size { dict.removeValue(forKey: Box(i)) }
    check(dict.isEmpty)
  }
}

func filter(n: Int) {
  for _ in 1...1000*n {
    let dict = numberMap
    let result = dict.filter {key, value in value % 2 == 0}
    check(result.count == size/2)
  }
}

func filterObjects(n: Int) {
  for _ in 1...1000*n {
    let dict = boxedNumMap
    let result = dict.filter {key, value in value.value % 2 == 0}
    check(result.count == size/2)
  }
}