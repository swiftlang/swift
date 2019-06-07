//===--- ReplaceSubrange.swift -------------------------------------------===//
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

public let ReplaceSubrange = [
  BenchmarkInfo(name: "ReplaceSubrangeWithSmallLiteralString", runFunction: run_ReplaceSubrangeWithSmallLiteralString, tags: [.validation, .api]),
  BenchmarkInfo(name: "ReplaceSubrangeWithLargeLiteralString", runFunction: run_ReplaceSubrangeWithLargeLiteralString, tags: [.validation, .api]),
  BenchmarkInfo(name: "ReplaceSubrangeWithLargeManagedString", runFunction: run_ReplaceSubrangeWithLargeManagedString, tags: [.validation, .api], setUpFunction: setupLargeManagedString),
]

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


@inline(never)
public func run_ReplaceSubrangeWithSmallLiteralString(N: Int) {

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
public func run_ReplaceSubrangeWithLargeLiteralString(N: Int) {

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
public func run_ReplaceSubrangeWithLargeManagedString(N: Int) {

    for _ in 0 ..< N {
        var string = largeManagedString

        for _ in 0 ..< 10000 {
            let range = string.startIndex..<string.index(after: string.startIndex)
            let replacingString = "t"
            string.replaceSubrange(range, with: replacingString)
        }
    }
}

private func setupLargeManagedString() {
    _ = largeManagedString
}
