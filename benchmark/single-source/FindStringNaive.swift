//===--- FindStringNaive.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// Mini benchmark implementing a naive String search algorithm that
// at the moment shows a lot of ARC traffic.
let t: [BenchmarkCategory] = [.String, .refcount]
let N = 100

var longStringFoFoFoFox: String?
var longArrayFoFoFoFox: [UInt8]?

public let FindStringNaive = [
  BenchmarkInfo(
    name: "FindString.Loop1.Substring",
    runFunction: runBenchLoop1Substring,
    tags: t,
    setUpFunction: {
      longStringFoFoFoFox = String(repeating: "fo", count: 5_000) + "fox <-- needle"
    }),
  BenchmarkInfo(
    name: "FindString.Rec3.String",
    runFunction: runBenchRecursive3String,
    tags: t,
    setUpFunction: {
      longStringFoFoFoFox = String(repeating: "fo", count: 500) + "fox <-- needle"
    }),
  BenchmarkInfo(
    name: "FindString.Rec3.Substring",
    runFunction: runBenchRecursive3Substring,
    tags: t,
    setUpFunction: {
      longStringFoFoFoFox = String(repeating: "fo", count: 500) + "fox <-- needle"
    }),
  BenchmarkInfo(
    name: "FindString.Loop1.Array",
    runFunction: runBenchLoop1Array,
    tags: t,
    setUpFunction: {
      longArrayFoFoFoFox = []
      longArrayFoFoFoFox!.reserveCapacity(1_100_000)
      for _ in 0 ..< 500_000 {
        longArrayFoFoFoFox!.append(contentsOf: "fo".utf8)
      }
      longArrayFoFoFoFox!.append(contentsOf: "fox <-- needle".utf8)
    }),
  BenchmarkInfo(
    name: "FindString.Rec3.Array",
    runFunction: runBenchRecursive3ArrayOfUTF8,
    tags: t,
    setUpFunction: {
      longArrayFoFoFoFox = []
      longArrayFoFoFoFox!.reserveCapacity(11_000)
      for _ in 0 ..< 5_000 {
        longArrayFoFoFoFox!.append(contentsOf: "fo".utf8)
      }
      longArrayFoFoFoFox!.append(contentsOf: "fox <-- needle".utf8)
    }),
]

func findOne<S: StringProtocol>(
  _ string: S,
  needle: Character
) -> String.Index? {
  var index = string.startIndex
  while index < string.endIndex {
    let nextIndex = string.index(after: index)
    if string[index] == needle {
      return index
    }
    index = nextIndex
  }
  return nil
}

func findThreeRecursive<S: StringProtocol>(
  _ string: S,
  needle1: Character,
  needle2: Character?,
  needle3: Character?
) -> String.Index? {
  var index = string.startIndex
  while index < string.endIndex {
    let nextIndex = string.index(after: index)
    if string[index] == needle1 {
      // Check subsequent needles recursively (if applicable)
      guard let needle2 = needle2 else { return index }

      if findThreeRecursive(
        string[nextIndex...].prefix(2), needle1: needle2, needle2: needle3, needle3: nil
      ) == nextIndex {
        return index
      }
    }
    index = nextIndex
  }
  return nil
}

func findOneOnUTF8Collection<Bytes: Collection>(
  _ string: Bytes,
  needle: UInt8
) -> Bytes.Index? where Bytes.Element == UInt8 {
  var index = string.startIndex
  while index < string.endIndex {
    let nextIndex = string.index(after: index)
    if string[index] == needle {
      return index
    }
    index = nextIndex
  }
  return nil
}

func findThreeOnUTF8Collection<Bytes: Collection>(
  _ string: Bytes,
  needle1: UInt8,
  needle2: UInt8?,
  needle3: UInt8?
) -> Bytes.Index? where Bytes.Element == UInt8 {
  var index = string.startIndex
  while index < string.endIndex {
    let nextIndex = string.index(after: index)
    if string[index] == needle1 {
      // Check subsequent needles recursively (if applicable)
      guard let needle2 = needle2 else { return index }

      if findThreeOnUTF8Collection(
        string[nextIndex...].prefix(2), needle1: needle2, needle2: needle3, needle3: nil
      ) == nextIndex {
        return index
      }
    }
    index = nextIndex
  }
  return nil
}

@inline(never)
func runBenchLoop1Substring(iterations: Int) {
  for _ in 0 ..< iterations {
    precondition(findOne(longStringFoFoFoFox![...], needle: "x") != nil)
  }
}

@inline(never)
func runBenchLoop1Array(iterations: Int) {
  for _ in 0 ..< iterations {
    precondition(findOneOnUTF8Collection(longArrayFoFoFoFox!, needle: UInt8(ascii: "x")) != nil)
  }
}

@inline(never)
func runBenchRecursive3Substring(iterations: Int) {
  for _ in 0 ..< iterations {
    precondition(findThreeRecursive(longStringFoFoFoFox![...], needle1: "f", needle2: "o", needle3: "x") != nil)
  }
}

@inline(never)
func runBenchRecursive3String(iterations: Int) {
  for _ in 0 ..< iterations {
    precondition(findThreeRecursive(longStringFoFoFoFox!, needle1: "f", needle2: "o", needle3: "x") != nil)
  }
}

@inline(never)
func runBenchRecursive3ArrayOfUTF8(iterations: Int) {
  for _ in 0 ..< iterations {
    precondition(findThreeOnUTF8Collection(longArrayFoFoFoFox!,
                                      needle1: UInt8(ascii: "f"),
                                      needle2: UInt8(ascii: "o"),
                                      needle3: UInt8(ascii: "x")) != nil)
  }
}
