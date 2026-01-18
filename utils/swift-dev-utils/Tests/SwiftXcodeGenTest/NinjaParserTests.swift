//===--- NinjaParserTests.swift -------------------------------------------===//
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

import Foundation
import Testing

@testable import SwiftXcodeGen

fileprivate func expectEqual<T, U: Equatable>(
  _ expected: T,
  _ actual: T,
  _ kp: KeyPath<T, U>,
  sourceLocation: SourceLocation = #_sourceLocation
) {
  #expect(
    expected[keyPath: kp] == actual[keyPath: kp],
    sourceLocation: sourceLocation
  )
}

fileprivate func assertParse(
  _ str: String,
  bindings: [String: String] = [:],
  rules: [String: NinjaBuildFile.Rule] = [:],
  edges: [NinjaBuildFile.BuildEdge],
  sourceLocation: SourceLocation = #_sourceLocation
) {
  let filePath: AbsolutePath = "/tmp/build.ninja"
  let files: [AbsolutePath: String] = [
    filePath: str
  ]
  assertParse(
    filePath,
    in: files,
    bindings: bindings,
    rules: rules,
    edges: edges,
    sourceLocation: sourceLocation
  )
}

fileprivate func assertParse(
  _ filePath: AbsolutePath,
  in fileSystem: [AbsolutePath: String],
  bindings: [String: String] = [:],
  rules: [String: NinjaBuildFile.Rule] = [:],
  edges: [NinjaBuildFile.BuildEdge],
  sourceLocation: SourceLocation = #_sourceLocation
) {
  do {
    let buildFile = try NinjaParser.parse(filePath: filePath, fileReader: { Data(fileSystem[$0]!.utf8) })
    guard edges.count == buildFile.buildEdges.count else {
      Issue.record(
        "Expected \(edges.count) edges, got \(buildFile.buildEdges.count)",
        sourceLocation: sourceLocation
      )
      return
    }
    #expect(
      bindings == buildFile.bindings.values,
      sourceLocation: sourceLocation
    )
    #expect(
      rules == buildFile.rules,
      sourceLocation: sourceLocation
    )
    for (expected, actual) in zip(edges, buildFile.buildEdges) {
      expectEqual(expected, actual, \.ruleName, sourceLocation: sourceLocation)
      expectEqual(expected, actual, \.inputs, sourceLocation: sourceLocation)
      expectEqual(expected, actual, \.outputs, sourceLocation: sourceLocation)
      expectEqual(expected, actual, \.dependencies, sourceLocation: sourceLocation)
      expectEqual(expected, actual, \.bindings, sourceLocation: sourceLocation)

      #expect(expected == actual, sourceLocation: sourceLocation)
    }
  } catch {
    Issue.record("\(error)", sourceLocation: sourceLocation)
  }
}

@Suite
struct NinjaParserTests {
  @Test
  func buildEdge() throws {
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

  @Test
  func rule() throws {
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
          ])
      ],
      edges: []
    )
  }

  @Test
  func include() throws {
    let files: [AbsolutePath: String] = [
      "/tmp/build.ninja": """
        include path/to/sub.ninja
        
        build foo.swiftmodule : SWIFTC foo.swift
        """,
      "/tmp/path/to/sub.ninja": """
        rule SWIFTC
          command = /bin/swiftc $in -o $out
        """
    ]
    assertParse(
      "/tmp/build.ninja",
      in: files,
      rules: [
        "SWIFTC": .init(
          name: "SWIFTC",
          bindings: [
            "command": "/bin/swiftc $in -o $out",
          ])
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

  @Test
  func phonyRule() throws {
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

  @Test
  func bindings() throws {
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
        "FLAGS": "-I /c/d -wmo"
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
            "COMMAND": "/bin/swiftc -I /a/b -wmo",
          ]
        )
      ]
    )
  }

  @Test
  func escape() throws {
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
