//===--- SmallStringCompatibility.swift -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test
// REQUIRES: CPU=wasm32 || CPU=arm64_32

import StdlibUnittest

var suite = TestSuite("SmallStringCompatibility")
defer { runAllTests() }

var strings = [
  ("Small", true),
  ("Less small", true),
  ("Positively large.", true),
]

#if os(watchOS)
strings[1].1 = false
#endif

strings.forEach { (string, contiguous) in
  suite.test("Contiguous: \(string)")
  .require(.stdlib_6_2).code {

    expectEqual(string.isContiguousUTF8, contiguous)
  }
}

strings.forEach { (string, contiguous) in
  suite.test("Contiguous Substring: \(string)")
  .require(.stdlib_6_2).code {
    let substring = string[...]
    expectEqual(substring.isContiguousUTF8, contiguous)
  }
}

strings.forEach { (string, contiguous) in
  suite.test("String.makeContiguousUTF8: \(string)")
  .require(.stdlib_6_2).code {
    var s = string
    s.makeContiguousUTF8()
    expectTrue(s.isContiguousUTF8)
    expectEqual(s, string)
  }
}

strings.forEach { (string, contiguous) in
  suite.test("Substring.makeContiguousUTF8: \(string)")
  .require(.stdlib_6_2).code {
    var s: Substring = string.dropFirst().dropLast()
    s.makeContiguousUTF8()
    expectTrue(s.isContiguousUTF8)
    expectEqual(s, string.dropFirst().dropLast())
  }
}
