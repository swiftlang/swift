//===--- ScannerTests.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import XCTest

@testable import SwiftXcodeGen

class ScannerTests: XCTestCase {
  func testReplacement() {
    // Currently implemented using BinaryScanner for ASCII cases.
    XCTAssertEqual("b", "a".replacing("a", with: "b"))
    XCTAssertEqual("bbbb", "abaa".replacing("a", with: "b"))
    XCTAssertEqual("a", "a".replacing("aaaa", with: "b"))
    XCTAssertEqual("cca", "ababa".replacing("ab", with: "c"))
    XCTAssertEqual("ccbccbcc", "ababa".replacing("a", with: "cc"))
  }
}
