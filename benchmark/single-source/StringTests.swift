//===--- StringTests.swift ------------------------------------------------===//
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

public func run_StringWithCString(_ N: Int) {
  let str = String(repeating: "x", count: 100 * (1 << 16))
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

@inline(never)
internal func compareEqual(_ str1: String, _ str2: String) -> Bool {
  return str1 == str2
}

public func run_StringEqualPointerComparison(_ N: Int) {
  let str1 = "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. "
  let str2 = str1
  for _ in 0 ..< N {
    for _ in 0 ..< 100_000 {
      if !compareEqual(str1, str2) {
        CheckResults(false, "Strings should be equal")
      }
    }
  }
}
