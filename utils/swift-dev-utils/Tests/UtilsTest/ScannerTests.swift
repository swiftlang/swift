//===--- ScannerTests.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Testing

@testable import Utils

@Suite
struct ScannerTests {
  @Test
  func replacement() {
    // Currently implemented using BinaryScanner for ASCII cases.
    #expect("b" == "a".replacing("a", with: "b"))
    #expect("bbbb" == "abaa".replacing("a", with: "b"))
    #expect("a" == "a".replacing("aaaa", with: "b"))
    #expect("cca" == "ababa".replacing("ab", with: "c"))
    #expect("ccbccbcc" == "ababa".replacing("a", with: "cc"))
  }
}
