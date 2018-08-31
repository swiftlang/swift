//===--- StringBuilder.swift ----------------------------------------------===//
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

public let StringBuilder = [
  BenchmarkInfo(
    name: "StringAdder",
    runFunction: run_StringAdder,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringBuilder",
    runFunction: run_StringBuilder,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringBuilderSmallReservingCapacity",
    runFunction: run_StringBuilderSmallReservingCapacity,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringUTF16Builder",
    runFunction: run_StringUTF16Builder,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringUTF16SubstringBuilder",
    runFunction: run_StringUTF16SubstringBuilder,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringBuilderLong",
    runFunction: run_StringBuilderLong,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringBuilderWithLongSubstring",
    runFunction: run_StringBuilderWithLongSubstring,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringWordBuilder",
    runFunction: run_StringWordBuilder,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringWordBuilderReservingCapacity",
    runFunction: run_StringWordBuilderReservingCapacity,
    tags: [.validation, .api, .String]),
]

@inline(never)
func buildString(_ i: String, reservingCapacity: Bool = false) -> String {
  var sb = getString(i)
  if reservingCapacity {
    sb.reserveCapacity(10)
  }
  for str in ["b","c","d","pizza"] {
    sb += str
  }
  return sb
}

@inline(never)
public func run_StringBuilder(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildString("a"))
  }
}

@inline(never)
public func run_StringBuilderSmallReservingCapacity(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildString("a", reservingCapacity: true))
  }
}

@inline(never)
func addString(_ i: String) -> String {
  let s = getString(i) + "b" + "c" + "d" + "pizza"
  return s
}

@inline(never)
public func run_StringAdder(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(addString("a"))
  }
}

@inline(never)
func buildStringUTF16(_ i: String) -> String {
  var sb = getString(i)
  for str in ["🎉","c","d","pizza"] {
    sb += str
  }
  return sb
}

@inline(never)
func buildStringFromSmallSubstrings(_ i: String) -> String {
  var sb = getString(i)
  for str in ["_🎉","cd","de","pizza"] {
    sb += str.dropFirst()
  }
  return sb
}

@inline(never)
public func run_StringUTF16Builder(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildStringUTF16("a"))
  }
}

@inline(never)
public func run_StringUTF16SubstringBuilder(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildStringFromSmallSubstrings("a"))
  }
}

func getLongString() -> String {
  let long = """
    Swift is a multi-paradigm, compiled programming language created for
     iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is
     designed to work with Apple's Cocoa and Cocoa Touch frameworks and the
     large body of existing Objective-C code written for Apple products. Swift
     is intended to be more resilient to erroneous code (\"safer\") than
     Objective-C and also more concise. It is built with the LLVM compiler
     framework included in Xcode 6 and later and uses the Objective-C runtime,
     which allows C, Objective-C, C++ and Swift code to run within a single
     program.
    """
  return getString(long)
}

@inline(never)
func buildStringLong(_ i: String) -> String {
  var sb = getString(i)
  sb += getLongString()
  return sb
}

@inline(never)
func buildStringWithLongSubstring(_ i: String) -> String {
  var sb = getString(i)
  sb += getLongString().dropFirst()
  return sb
}

@inline(never)
public func run_StringBuilderLong(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildStringLong("👻"))
  }
}

@inline(never)
public func run_StringBuilderWithLongSubstring(_ N: Int) {
  for _ in 1...5000*N {
    blackHole(buildStringWithLongSubstring("👻"))
  }
}

@inline(never)
func buildString(
  word: String,
  count: Int,
  reservingCapacity: Bool
) -> String {
  let word = getString(word)
  var sb = ""
  if reservingCapacity {
    sb.reserveCapacity(count * word.unicodeScalars.count)
  }
  for _ in 0 ..< count {
    sb += word
  }
  return sb
}

@inline(never)
public func run_StringWordBuilder(_ N: Int) {
  blackHole(buildString(
    word: "bumfuzzle", count: 50_000 * N, reservingCapacity: false))
}

@inline(never)
public func run_StringWordBuilderReservingCapacity(_ N: Int) {
  blackHole(buildString(
    word: "bumfuzzle", count: 50_000 * N, reservingCapacity: true))
}

