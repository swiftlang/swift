//===--- NinjaParserTests.swift -------------------------------------------===//
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

fileprivate func expectEqual<T: Equatable>(
  expected: [T],
  actual: [T],
  description: String,
  file: StaticString = #file,
  line: UInt = #line
) {
  guard expected.count == actual.count else {
    XCTFail(
      """
      Expected \(expected.count) '\(description)', \
      got \(actual.count) (\(actual))
      """,
      file: file,
      line: line
    )
    return
  }
  for (expected, actual) in zip(expected, actual) {
    XCTAssertEqual(expected, actual, file: file, line: line)
  }
}

fileprivate func expectEqual<T, U: Equatable>(
  _ expected: T,
  _ actual: T,
  _ kp: KeyPath<T, U>,
  file: StaticString = #file,
  line: UInt = #line
) {
  XCTAssertEqual(
    expected[keyPath: kp],
    actual[keyPath: kp],
    file: file,
    line: line
  )
}

fileprivate func expectEqual<T, U: Equatable>(
  _ expected: T,
  _ actual: T,
  _ kp: KeyPath<T, [U]>,
  file: StaticString = #file,
  line: UInt = #line
) {
  expectEqual(
    expected: expected[keyPath: kp],
    actual: actual[keyPath: kp],
    description: "\(kp)",
    file: file,
    line: line
  )
}

fileprivate func assertParse(
  _ str: String,
  bindings: [String: String] = [:],
  rules: [String: NinjaBuildFile.Rule] = [:],
  edges: [NinjaBuildFile.BuildEdge],
  file: StaticString = #file,
  line: UInt = #line
) {
  let filePath: AbsolutePath = "/tmp/build.ninja"
  let files: [AbsolutePath: String] = [
    filePath: str
  ]
  assertParse(filePath, in: files, bindings: bindings, rules: rules, edges: edges, file: file, line: line)
}

fileprivate func assertParse(
  _ filePath: AbsolutePath,
  in fileSystem: [AbsolutePath: String],
  bindings: [String: String] = [:],
  rules: [String: NinjaBuildFile.Rule] = [:],
  edges: [NinjaBuildFile.BuildEdge],
  file: StaticString = #file,
  line: UInt = #line
) {
  do {
    let buildFile = try NinjaParser.parse(filePath: filePath, fileReader: { Data(fileSystem[$0]!.utf8) })
    guard edges.count == buildFile.buildEdges.count else {
      XCTFail(
        "Expected \(edges.count) edges, got \(buildFile.buildEdges.count)",
        file: file,
        line: line
      )
      return
    }
    XCTAssertEqual(
      bindings,
      buildFile.bindings.values,
      file: file,
      line: line
    )
    XCTAssertEqual(
      rules,
      buildFile.rules,
      file: file,
      line: line
    )
    for (expected, actual) in zip(edges, buildFile.buildEdges) {
      expectEqual(expected, actual, \.ruleName, file: file, line: line)
      expectEqual(expected, actual, \.inputs, file: file, line: line)
      expectEqual(expected, actual, \.outputs, file: file, line: line)
      expectEqual(expected, actual, \.dependencies, file: file, line: line)
      expectEqual(expected, actual, \.bindings, file: file, line: line)

      XCTAssertEqual(expected, actual, file: file, line: line)
    }
  } catch {
    XCTFail("\(error)", file: file, line: line)
  }
}

class NinjaParserTests: XCTestCase {
  func testBuildEdge() throws {
    assertParse(
      """
      # ignore comment, build foo.o: a.swift | dep || orderdep
      #another build comment
      build foo.o foo.swiftmodule: SWIFTC a.swift | dep || orderdep
      notpartofthebuildrule
      """,
      edges: [
        .init(
          ruleName: "SWIFTC",
          inputs: ["a.swift"],
          outputs: ["foo.o", "foo.swiftmodule"],
          dependencies: ["dep", "orderdep"],
          bindings: [:]
        )
      ]
    )
  }

  func testRule() throws {
    assertParse(
      """
      rule SWIFTC
        command = /bin/switfc -wmo -target unknown
        other = whatever
      notpartoftherule
      """,
      rules: [
        "SWIFTC": .init(
          name: "SWIFTC",
          bindings: [
            "command": "/bin/switfc -wmo -target unknown",
            "other": "whatever",
          ]
        )
      ],
      edges: []
    )
  }

  func testInclude() throws {
    let files: [AbsolutePath: String] = [
      "/tmp/build.ninja": """
      include path/to/sub.ninja

      build foo.swiftmodule : SWIFTC foo.swift
      """,
      "/tmp/path/to/sub.ninja": """
      rule SWIFTC
        command = /bin/swiftc $in -o $out
      """,
    ]
    assertParse(
      "/tmp/build.ninja",
      in: files,
      rules: [
        "SWIFTC": .init(
          name: "SWIFTC",
          bindings: [
            "command": "/bin/swiftc $in -o $out"
          ]
        )
      ],
      edges: [
        .init(
          ruleName: "SWIFTC",
          inputs: ["foo.swift"],
          outputs: ["foo.swiftmodule"],
          dependencies: [],
          bindings: [:]
        )
      ]
    )
  }

  func testPhonyRule() throws {
    assertParse(
      """
      build foo.swiftmodule : phony bar.swiftmodule
      """,
      edges: [
        .phony(
          for: ["foo.swiftmodule"],
          inputs: ["bar.swiftmodule"]
        )
      ]
    )
  }

  func testBindings() throws {
    assertParse(
      """
      x = y

      CONFIGURATION = Debug

      build foo.o: SWIFTC xyz foo.swift | baz.o
        UNKNOWN = foobar
        SWIFT_MODULE_NAME = foobar

        #ignore trivia between attributes

        \u{20}
        #ignore trivia between attributes

        FLAGS = -I /a/b -wmo
        ANOTHER_UNKNOWN = a b c

      build baz.o: CUSTOM_COMMAND baz.swift
        COMMAND = /bin/swiftc -I /a/b -wmo
      FLAGS = -I /c/d -wmo

      """,
      bindings: [
        "x": "y",

        "CONFIGURATION": "Debug",

        // This is considered top-level since it's not indented.
        "FLAGS": "-I /c/d -wmo",
      ],
      edges: [
        .init(
          ruleName: "SWIFTC",
          inputs: ["xyz", "foo.swift"],
          outputs: ["foo.o"],
          dependencies: ["baz.o"],
          bindings: [
            "UNKNOWN": "foobar",
            "SWIFT_MODULE_NAME": "foobar",
            "FLAGS": "-I /a/b -wmo",
            "ANOTHER_UNKNOWN": "a b c",
          ]
        ),
        .init(
          ruleName: "CUSTOM_COMMAND",
          inputs: ["baz.swift"],
          outputs: ["baz.o"],
          dependencies: [],
          bindings: [
            "COMMAND": "/bin/swiftc -I /a/b -wmo"
          ]
        ),
      ]
    )
  }

  func testEscape() throws {
    for newline in ["\n", "\r", "\r\n"] {
      assertParse(
        """
        build foo.o$:: SWIFTC xyz$ foo$$.swift | baz$ bar.o
          FLAGS = -I /a$\(newline)\
                  /b -wmo
          COMMAND = swiftc$$
        """,
        edges: [
          .init(
            ruleName: "SWIFTC",
            inputs: ["xyz foo$.swift"],
            outputs: ["foo.o:"],
            dependencies: ["baz bar.o"],
            bindings: [
              "FLAGS": "-I /a/b -wmo",
              "COMMAND": "swiftc$",
            ]
          )
        ]
      )
    }
  }
}
