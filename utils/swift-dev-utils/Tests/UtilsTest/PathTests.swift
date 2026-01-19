//===--- PathTests.swift --------------------------------------------------===//
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
struct PathTests {
  @Test
  func relativeParent() throws {
    #expect(RelativePath("").parentDir == nil)
    #expect(RelativePath("foo").parentDir == nil)
    #expect(RelativePath("foo/bar").parentDir == "foo")
  }

  @Test
  func absoluteParent() throws {
    #expect(AbsolutePath("/").parentDir == nil)
    #expect(AbsolutePath("/foo").parentDir == "/")
    #expect(AbsolutePath("/foo/bar").parentDir == "/foo")
  }

  @Test
  func dropLast() throws {
    #expect(AbsolutePath("/").dropLast() == "/")
    #expect(AbsolutePath("/foo/bar").dropLast() == "/foo")
    #expect(AbsolutePath("/foo/bar").dropLast(2) == "/")
    #expect(AbsolutePath("/foo/bar").dropLast(5) == "/")

    #expect(RelativePath("").dropLast() == "")
    #expect(RelativePath("foo/bar").dropLast() == "foo")
    #expect(RelativePath("foo/bar").dropLast(2) == "")
    #expect(RelativePath("foo/bar").dropLast(5) == "")
  }

  @Test
  func fileExtension() throws {
    func match(
      _ ext: FileExtension,
      with path: String,
      value: Bool = true,
      sourceLocation: SourceLocation = #_sourceLocation
    ) {
      #expect(path.hasExtension(ext) == value, sourceLocation: sourceLocation)
      #expect(AnyPath(path).hasExtension(ext) == value, sourceLocation: sourceLocation)
    }
    match(.swift, with: "x.swift")
    match(.swift, with: "/x.swift")
    match(.swift, with: ".swift", value: false)
    match(.swift, with: "/.swift", value: false)

    match(.swift, with: "x.SWIFT")
    match(.swift, with: "/x.SWIFT")
    match(.swift, with: ".SWIFT", value: false)
    match(.swift, with: "/.SWIFT", value: false)

    match(.swift, with: "x.swiftx", value: false)

    #expect("x.sWift".hasExtension(.asm, .swift))
    #expect(AnyPath("x.sWift").hasExtension(.asm, .swift))
  }
}
