//===--- StringTests.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import TestsUtils

public func run_StringWithCString(_ N: Int) {
  let str = String(repeating: "x" as UnicodeScalar, count: 100 * (1 << 16))
  for _ in 0 ..< N {
    str.withCString { _ in }
  }
}

public func run_StringHasPrefix(_ N: Int) {
  let prefix = "prefix"
  let testString = "prefixedString"
  for _ in 0 ..< N {
    for _ in 0 ..< 100_000 {
      if !testString.hasPrefix(prefix) {
        CheckResults(false, "prefix check failed")
      }
    }
  }
}

public func run_StringHasSuffix(_ N: Int) {
  let suffix = "Suffixed"
  let testString = "StringSuffixed"
  for _ in 0 ..< N {
    for _ in 0 ..< 100_000 {
      if !testString.hasSuffix(suffix) {
        CheckResults(false, "suffix check failed")
      }
    }
  }
}

public func run_StringHasPrefixUnicode(_ N: Int) {
  let prefix = "❄️prefix"
  let testString = "❄️prefixedString"
  for _ in 0 ..< N {
    for _ in 0 ..< 100_000 {
      if !testString.hasPrefix(prefix) {
        CheckResults(false, "prefix check failed")
      }
    }
  }
}

public func run_StringHasSuffixUnicode(_ N: Int) {
  let suffix = "❄️Suffixed"
  let testString = "String❄️Suffixed"
  for _ in 0 ..< N {
    for _ in 0 ..< 100_000 {
      if !testString.hasSuffix(suffix) {
        CheckResults(false, "suffix check failed")
      }
    }
  }
}
