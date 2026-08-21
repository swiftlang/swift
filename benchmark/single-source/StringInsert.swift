//===--- StringInsert.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// Benchmarks for `String.insert(contentsOf:at:)`, the RangeReplaceableCollection
// insertion that takes an arbitrary Collection of Characters. Every payload
// below inserts the same three characters, so the numbers only differ in the
// type of the collection being inserted from.
//
// String, Substring and Array<Character> have `@_specialize` attributes on
// `insert(contentsOf:at:)`; Repeated<Character> does not, so it covers the
// unspecialized generic path. That path is more than an order of magnitude
// slower, so the Repeated<Character> benchmarks perform fewer insertions per
// iteration to keep every benchmark inside the runtime window that
// `Benchmark_Driver check` expects. Divide by `specialized`/`unspecialized`
// before comparing numbers across payload types.
//
// The single-`Character` overload is covered by InsertCharacter.swift.

let tags: [BenchmarkCategory] = [.validation, .api, .String, .cpubench]

public let benchmarks = [
  BenchmarkInfo(
    name: "String.insertContentsOf.String.Small",
    runFunction: { insert($0, smallString, with: stringPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.String",
    runFunction: { insert($0, largeString, with: stringPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.Substring.Small",
    runFunction: { insert($0, smallString, with: substringPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.Substring",
    runFunction: { insert($0, largeString, with: substringPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.ArrChar.Small",
    runFunction: { insert($0, smallString, with: arrayPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.ArrChar",
    runFunction: { insert($0, largeString, with: arrayPayload, count: specialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.RepChar.Small",
    runFunction: { insert($0, smallString, with: repeatedPayload, count: unspecialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
  BenchmarkInfo(
    name: "String.insertContentsOf.RepChar",
    runFunction: { insert($0, largeString, with: repeatedPayload, count: unspecialized) },
    tags: tags,
    setUpFunction: buildWorkload
  ),
]

// Insertions performed per iteration of `n`, picked so that every benchmark
// lands well inside the 20–1000 μs window checked by `Benchmark_Driver check`.
let specialized = 3000
let unspecialized = 900

let smallString = "coffee"
let largeString = "coffee\u{301}coffeecoffeecoffeecoffee"

let stringPayload = "ttt"
let substringPayload = "ttt"[...]
let arrayPayload = Array<Character>(["t", "t", "t"])
let repeatedPayload = repeatElement(Character("t"), count: 3)

// Force the lazy initialization of the workloads out of the measured region.
func buildWorkload() {
  blackHole(smallString)
  blackHole(largeString)
  blackHole(stringPayload)
  blackHole(substringPayload)
  blackHole(arrayPayload)
  blackHole(repeatedPayload)
}

// Insert at the start index, so the existing contents have to be shifted out of
// the way. Each iteration starts from a fresh copy: repeatedly inserting into
// the same string would make it grow without bound, turning the workload
// super-linear in `n` and pushing the small-string cases out of small form
// after the first few insertions. The copy costs the same for every payload
// type, so the comparison between them still holds.
@inline(never)
private func insert<C: Collection>(
  _ n: Int, _ string: String, with newElements: C, count: Int
) where C.Element == Character {
  for _ in 0 ..< count * n {
    var copy = getString(string)
    copy.insert(contentsOf: newElements, at: copy.startIndex)
    blackHole(copy)
  }
}
