//===--- NSStringAPI.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation

@inline(never)
public func run_String_dataUsingEncoding(_ N: Int) {
    let string = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string.data(using: .utf8)
    }
}

@inline(never)
public func run_String_canBeConverted_utf8(_ N: Int) {
    let string = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string.canBeConverted(to: .utf8)
    }
}

@inline(never)
public func run_String_canBeConverted_utf16(_ N: Int) {
    let string = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string.canBeConverted(to: .utf16)
    }
}

@inline(never)
public func run_String_canBeConverted_ascii(_ N: Int) {
    let string = String(repeating: "🤗", count: 4096)
    for _ in 1...N*100 {
        _ = string.canBeConverted(to: .ascii)
    }
}

@inline(never)
public func run_String_capitalized(_ N: Int) {
    let string = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string.capitalized
    }
}

@inline(never)
public func run_String_localizedCapitalized(_ N: Int) {
    if #available(OSX 10.11, iOS 9.0, *) {
        let string = String(repeating: "x", count: 4096)
        for _ in 1...N*100 {
            _ = string.localizedCapitalized
        }
    }
}

@inline(never)
public func run_String_caseInsensitiveCompare_equal(_ N: Int) {
    let string1 = String(repeating: "x", count: 4096)
    let string2 = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string1.caseInsensitiveCompare(string2)
    }
}

@inline(never)
public func run_String_caseInsensitiveCompare_caseDiffering(_ N: Int) {
    let string1 = String(repeating: "x", count: 4096)
    let string2 = String(repeating: "X", count: 4096)
    for _ in 1...N*100 {
        _ = string1.caseInsensitiveCompare(string2)
    }
}

@inline(never)
public func run_String_caseInsensitiveCompare_notEqual(_ N: Int) {
    let string1 = String(repeating: "x", count: 4096)
    let string2 = String(repeating: "y", count: 4096)
    for _ in 1...N*100 {
        _ = string1.caseInsensitiveCompare(string2)
    }
}

@inline(never)
public func run_String_commonPrefix_sharedPrefix(_ N: Int) {
    let string1 = "hello" + String(repeating: "x", count: 4096)
    let string2 = "hello" + String(repeating: "y", count: 4096)
    for _ in 1...N*100 {
        _ = string1.commonPrefix(with: string2)
    }
}

@inline(never)
public func run_String_commonPrefix_notShared(_ N: Int) {
    let string1 = "goodbye" + String(repeating: "x", count: 4096)
    let string2 = "hello" + String(repeating: "y", count: 4096)
    for _ in 1...N*100 {
        _ = string1.commonPrefix(with: string2)
    }
}

@inline(never)
public func run_String_commonPrefix_sharedPrefix_caseInsensitive(_ N: Int) {
    let string1 = "hello" + String(repeating: "x", count: 4096)
    let string2 = "HELLo" + String(repeating: "y", count: 4096)
    for _ in 1...N*100 {
        _ = string1.commonPrefix(with: string2, options: .caseInsensitive)
    }
}

@inline(never)
public func run_String_components_CharacterSet(_ N: Int) {
    let string1 = String(repeating: "x\n", count: 4096)
    for _ in 1...N*100 {
        _ = string1.components(separatedBy: .newlines)
    }
}

@inline(never)
public func run_String_components_String(_ N: Int) {
    let string1 = String(repeating: "x- test -", count: 4096)
    for _ in 1...N*100 {
        _ = string1.components(separatedBy: "- test -")
    }
}

@inline(never)
public func run_String_enumerateLines(_ N: Int) {
    let string1 = String(repeating: "x\n", count: 4096)
    for _ in 1...N*100 {
        _ = string1.enumerateLines { (_, _) in }
    }
}

@inline(never)
public func run_String_fastestEncoding_ascii(_ N: Int) {
    let string1 = String(repeating: "x", count: 4096)
    for _ in 1...N*100 {
        _ = string1.fastestEncoding
    }
}

@inline(never)
public func run_String_fastestEncoding_unicode(_ N: Int) {
    let string1 = String(repeating: "💅🏾", count: 4096)
    for _ in 1...N*100 {
        _ = string1.fastestEncoding
    }
}

