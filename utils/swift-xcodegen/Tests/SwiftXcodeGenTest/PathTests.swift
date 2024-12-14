//===--- PathTests.swift --------------------------------------------------===//
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

class PathTests: XCTestCase {
  func testRelativeParent() throws {
    XCTAssertEqual(RelativePath("").parentDir, nil)
    XCTAssertEqual(RelativePath("foo").parentDir, nil)
    XCTAssertEqual(RelativePath("foo/bar").parentDir, "foo")
  }

  func testAbsoluteParent() throws {
    XCTAssertEqual(AbsolutePath("/").parentDir, nil)
    XCTAssertEqual(AbsolutePath("/foo").parentDir, "/")
    XCTAssertEqual(AbsolutePath("/foo/bar").parentDir, "/foo")
  }

  func testDropLast() throws {
    XCTAssertEqual(AbsolutePath("/").dropLast(), "/")
    XCTAssertEqual(AbsolutePath("/foo/bar").dropLast(), "/foo")
    XCTAssertEqual(AbsolutePath("/foo/bar").dropLast(2), "/")
    XCTAssertEqual(AbsolutePath("/foo/bar").dropLast(5), "/")

    XCTAssertEqual(RelativePath("").dropLast(), "")
    XCTAssertEqual(RelativePath("foo/bar").dropLast(), "foo")
    XCTAssertEqual(RelativePath("foo/bar").dropLast(2), "")
    XCTAssertEqual(RelativePath("foo/bar").dropLast(5), "")
  }
}
