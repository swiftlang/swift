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

public let StringReplaceSubrange = [
  BenchmarkInfo(name: "Str.replaceSubrange.SmallLiteralString", runFunction: run_StrReplaceSubrangeSmallLiteralString, tags: [.validation, .api]),
  BenchmarkInfo(name: "Str.replaceSubrange.LargeLiteralString", runFunction: run_StrReplaceSubrangeLargeLiteralString, tags: [.validation, .api]),
  BenchmarkInfo(name: "Str.replaceSubrange.LargeManagedString", runFunction: run_StrReplaceSubrangeLargeManagedString, tags: [.validation, .api], setUpFunction: setupLargeManagedString),
  BenchmarkInfo(name: "Str.replaceSubrange.SmallLiteralSubstr", runFunction: run_StrReplaceSubrangeSmallLiteralSubstr, tags: [.validation, .api]),
  BenchmarkInfo(name: "Str.replaceSubrange.LargeLiteralSubstr", runFunction: run_StrReplaceSubrangeLargeLiteralSubstr, tags: [.validation, .api]),
  BenchmarkInfo(name: "Str.replaceSubrange.LargeManagedSubstr", runFunction: run_StrReplaceSubrangeLargeManagedSubstr, tags: [.validation, .api], setUpFunction: setupLargeManagedSubstring),
]

// MARK: - Privates for String

private func smallLiteralString() -> String {
    return getString("coffee")
}

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

private func smallLiteralSubstring() -> Substring {
    return getSubstring("coffee")
}

private func largeLiteralSubstring() -> Substring {
    return getSubstring("coffee\u{301}coffeecoffeecoffeecoffee")
}

private var largeManagedSubstring: Substring = {
    var substring = largeLiteralSubstring()
    substring += "z"
    return substring
}()

private func setupLargeManagedSubstring() {
    _ = largeManagedString
}

@inline(never)
public func run_StrReplaceSubrangeSmallLiteralString(N: Int) {

    for _ in 0 ..< N {
        var string = smallLiteralString()

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

@inline(never)
public func run_StrReplaceSubrangeLargeLiteralString(N: Int) {

    for _ in 0 ..< N {
        var string = largeLiteralString()

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

@inline(never)
public func run_StrReplaceSubrangeLargeManagedString(N: Int) {

    for _ in 0 ..< N {
        var string = largeManagedString

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

@inline(never)
public func run_StrReplaceSubrangeSmallLiteralSubstr(N: Int) {

    for _ in 0 ..< N {
        var string = smallLiteralSubstring()

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

@inline(never)
public func run_StrReplaceSubrangeLargeLiteralSubstr(N: Int) {

    for _ in 0 ..< N {
        var string = largeLiteralSubstring()

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

@inline(never)
public func run_StrReplaceSubrangeLargeManagedSubstr(N: Int) {

    for _ in 0 ..< N {
        var string = largeManagedSubstring

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}
