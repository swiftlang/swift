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

@inline(never)
func buildString(_ i: String) -> String {
  var sb = i
  for str in ["b","c","d","pizza"] {
    sb += str
  }
  return sb
}

@inline(never)
public func run_StringBuilder(_ N: Int) {
  for _ in 1...5000*N {
    _ = buildString(getString("a"))
  }
}

@inline(never)
func addString(_ i: String) -> String {
  let s = i + "b" + "c" + "d" + "pizza"
  return s
}

@inline(never)
public func run_StringAdder(_ N: Int) {
  for _ in 1...5000*N {
    _ = addString(getString("a"))
  }
}

@inline(never)
func buildStringUTF16(_ i: String) -> String {
  var sb = i
  for str in ["🎉","c","d","pizza"] {
    sb += str
  }
  return sb
}

@inline(never)
public func run_StringUTF16Builder(_ N: Int) {
  for _ in 1...5000*N {
    _ = buildStringUTF16("a")
  }
}


@inline(never)
func buildStringLong(_ i: String) -> String {
  var sb = i
  let long = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."
  sb += long
  return sb
}



@inline(never)
public func run_StringBuilderLong(_ N: Int) {
  for _ in 1...5000*N {
    _ = buildStringLong("👻")
  }
}

