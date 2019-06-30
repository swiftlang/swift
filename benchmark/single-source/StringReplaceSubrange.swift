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
    name: "Str.replaceSubrange.SmallLiteralString",
    runFunction: { replaceSubrange($0, "coffee", with: "t") },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeLiteralString",
    runFunction: { replaceSubrange($0, "coffee\u{301}coffeecoffeecoffee", with: "t") },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManagedString",
    runFunction: { replaceSubrange($0, largeManagedString, with: "t") },
    tags: tags,
    setUpFunction: setupLargeManagedString
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.SmallLiteralSubstr",
    runFunction: { replaceSubrange($0, "coffee", with: getSubstring("t")) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeLiteralSubstr",
    runFunction: { replaceSubrange($0, "coffee\u{301}coffeecoffeecoffee", with: getSubstring("t")) },
    tags: tags
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManagedSubstr",
    runFunction: { replaceSubrange($0, largeManagedString, with: getSubstring("t")) },
    tags: tags,
    setUpFunction: setupLargeManagedSubstring
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.SmallLiteralArrChar",
    runFunction: { replaceSubrange($0, "coffee", with: Array<Character>(["t"])) },
    tags: tags,
    setUpFunction: setupLargeManagedSubstring
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeLiteralArrChar",
    runFunction: { replaceSubrange($0, "coffee", with: Array<Character>(["t"])) },
    tags: tags,
    setUpFunction: setupLargeManagedSubstring
  ),
  BenchmarkInfo(
    name: "Str.replaceSubrange.LargeManagedArrChar",
    runFunction: { replaceSubrange($0, "coffee", with: Array<Character>(["t"])) },
    tags: tags,
    setUpFunction: setupLargeManagedSubstring
  ),
]

// MARK: - Privates for String

private func largeLiteralString() -> String {
    return getString("coffee\u{301}coffeecoffeecoffeecoffee")
}

private var largeManagedString: String = {
    var str = largeLiteralString()
    str += "z"
    return str
}()

private func setupLargeManagedString() {
    _ = largeManagedString
}

// MARK: - Privates for Substring

private func largeLiteralSubstring() -> Substring {
    return getSubstring("coffee\u{301}coffeecoffeecoffeecoffee")
}

private var largeManagedSubstring: Substring = {
    var substring = largeLiteralSubstring()
    substring += "z"
    return substring
}()

private func setupLargeManagedSubstring() {
    _ = largeManagedSubstring
}

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
