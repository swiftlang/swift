//===--- StringReplaceSubrange.swift -------------------------------------------===//
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

import TestsUtils

let tags: [BenchmarkCategory] = [.validation, .api, .String]

public let StringReplaceSubrange = [
  BenchmarkInfo(
    name: "String.replaceSubrange.String.Small",
    runFunction: { replaceSubrange($0, smallString, with: "t") },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.String",
    runFunction: { replaceSubrange($0, largeString, with: "t") },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.Substring.Small",
    runFunction: { replaceSubrange($0, smallString, with: "t"[...]) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.Substring",
    runFunction: { replaceSubrange($0, largeString, with: "t"[...]) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.ArrChar.Small",
    runFunction: { replaceSubrange($0, smallString, with: arrayCharacter) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.ArrChar",
    runFunction: { replaceSubrange($0, largeString, with: arrayCharacter) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.RepChar.Small",
    runFunction: { replaceSubrange($0, smallString, with: repeatedCharacter) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "String.replaceSubrange.RepChar",
    runFunction: { replaceSubrange($0, largeString, with: repeatedCharacter) },
    tags: tags
  ),
]

let smallString = "coffee"
let largeString = "coffee\u{301}coffeecoffeecoffeecoffee"

let arrayCharacter = Array<Character>(["t"])
let repeatedCharacter = repeatElement(Character("t"), count: 1)

@inline(never)
private func replaceSubrange<C: Collection>(
  _ N: Int, _ string: String, with newElements: C
) where C.Element == Character {
    var copy = getString(string)
    let range = string.startIndex..<string.index(after: string.startIndex)
    for _ in 0 ..< 500 * N {
      copy.replaceSubrange(range, with: newElements)
    }
}
