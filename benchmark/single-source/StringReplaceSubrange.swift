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
    name: "Str.replaceSubrange.SmallLiteral.String",
    runFunction: { replaceSubrange($0, "coffee", with: "t") },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManaged.String",
    runFunction: { replaceSubrange($0, largeManagedString, with: "t") },
    tags: tags,
    setUpFunction: setupLargeManagedString
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.SmallLiteral.Substr",
    runFunction: { replaceSubrange($0, "coffee", with: getSubstring("t")) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManaged.Substr",
    runFunction: { replaceSubrange($0, largeManagedString, with: getSubstring("t")) },
    tags: tags,
    setUpFunction: setupLargeManagedString
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.SmallLiteral.ArrChar",
    runFunction: { replaceSubrange($0, "coffee", with: getArrayCharacter(Array<Character>(["t"]))) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManaged.ArrChar",
    runFunction: { replaceSubrange($0, largeManagedString, with: getArrayCharacter(Array<Character>(["t"]))) },
    tags: tags,
    setUpFunction: setupLargeManagedString
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.SmallLiteral.RepeatedChar",
    runFunction: { replaceSubrange($0, "coffee", with: getRepeatedCharacter(repeatedCharacter)) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManaged.RepeatedChar",
    runFunction: { replaceSubrange($0, largeManagedString, with: getRepeatedCharacter(repeatedCharacter)) },
    tags: tags,
    setUpFunction: setupLargeManagedString
  ),
]

// MARK: - Privates for String

private var largeManagedString: String = {
    return getString("coffee\u{301}coffeecoffeecoffeecoffee")
}()

private func setupLargeManagedString() {
    _ = largeManagedString
}

// MARK: - Privates for Repeated<Character>

private let repeatedCharacter: Repeated<Character> = {
  let character = Character("c")
  return repeatElement(character, count: 1)
}()


@inline(never)
private func replaceSubrange(_ N: Int, _ string: String, with replacingString: String) {
  var copy = getString(string)
  let range = string.startIndex..<string.index(after: string.startIndex)
  for _ in 0 ..< 5_000 * N {
    copy.replaceSubrange(range, with: replacingString)
  }
}

@inline(never)
private func replaceSubrange(_ N: Int, _ string: String, with replacingSubstring: Substring) {
  var copy = getString(string)
  let range = string.startIndex..<string.index(after: string.startIndex)
  for _ in 0 ..< 5_000 * N {
    copy.replaceSubrange(range, with: replacingSubstring)
  }
}

@inline(never)
private func replaceSubrange(_ N: Int, _ string: String, with replacingArrayCharacter: Array<Character>) {
  var copy = getString(string)
  let range = string.startIndex..<string.index(after: string.startIndex)
  for _ in 0 ..< 5_000 * N {
    copy.replaceSubrange(range, with: replacingArrayCharacter)
  }
}

@inline(never)
private func replaceSubrange(_ N: Int, _ string: String, with replacingRepeatedCharacter: Repeated<Character>) {
    var copy = getString(string)
    let range = string.startIndex..<string.index(after: string.startIndex)
    for _ in 0 ..< 5_000 * N {
        copy.replaceSubrange(range, with: replacingRepeatedCharacter)
    }
}
