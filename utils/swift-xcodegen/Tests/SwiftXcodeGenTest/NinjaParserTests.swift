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
  expected: [T], actual: [T], description: String,
  file: StaticString = #file, line: UInt = #line
) {
  guard expected.count == actual.count else {
    XCTFail(
      """
      Expected \(expected.count) '\(description)', \
      got \(actual.count) (\(actual))
      """,
      file: file, line: line
    )
    return
  }
  for (expected, actual) in zip(expected, actual) {
    XCTAssertEqual(expected, actual, file: file, line: line)
  }
}

fileprivate func expectEqual<T, U: Equatable>(
  _ expected: T, _ actual: T, _ kp: KeyPath<T, U>,
  file: StaticString = #file, line: UInt = #line
) {
  XCTAssertEqual(
    expected[keyPath: kp], actual[keyPath: kp], file: file, line: line
  )
}

fileprivate func expectEqual<T, U: Equatable>(
  _ expected: T, _ actual: T, _ kp: KeyPath<T, [U]>,
  file: StaticString = #file, line: UInt = #line
) {
  expectEqual(
    expected: expected[keyPath: kp], actual: actual[keyPath: kp],
    description: "\(kp)", file: file, line: line
  )
}

fileprivate func assertParse(
  _ str: String,
  attributes: [NinjaBuildFile.Attribute] = [],
  rules: [NinjaBuildFile.BuildRule],
  file: StaticString = #file, line: UInt = #line
) {
  do {
    let buildFile = try NinjaParser.parse(Data(str.utf8))
    guard rules.count == buildFile.buildRules.count else {
      XCTFail(
        "Expected \(rules.count) rules, got \(buildFile.buildRules.count)",
        file: file, line: line
      )
      return
    }
    XCTAssertEqual(
      Dictionary(uniqueKeysWithValues: attributes.map { ($0.key, $0) }), 
      buildFile.attributes,
      file: file, line: line
    )
    for (expected, actual) in zip(rules, buildFile.buildRules) {
      expectEqual(expected, actual, \.inputs, file: file, line: line)
      expectEqual(expected, actual, \.outputs, file: file, line: line)
      expectEqual(expected, actual,  \.dependencies, file: file, line: line)
      expectEqual(expected, actual,  \.attributes, file: file, line: line)
      expectEqual(expected, actual,  \.isPhony, file: file, line: line)

      XCTAssertEqual(expected, actual, file: file, line: line)
    }
  } catch {
    XCTFail("\(error)", file: file, line: line)
  }
}

class NinjaParserTests: XCTestCase {
  func testBuildRule() throws {
    assertParse("""
      # ignore comment, build foo.o: a.swift | dep || orderdep
      #another build comment
      build foo.o foo.swiftmodule: a.swift | dep || orderdep
      notpartofthebuildrule
      """, rules: [
        .init(
          inputs: ["a.swift"],
          outputs: ["foo.o", "foo.swiftmodule"],
          dependencies: ["dep", "orderdep"],
          attributes: [:]
        )
      ]
    )
  }

  func testPhonyRule() throws {
    assertParse("""
      build foo.swiftmodule : phony bar.swiftmodule
      """, rules: [
        .phony(
          for: ["foo.swiftmodule"],
          inputs: ["bar.swiftmodule"]
        )
      ]
    )
  }

  func testAttributes() throws {
    assertParse("""
      x = y

      CONFIGURATION = Debug

      build foo.o: xyz foo.swift | baz.o
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

      """, attributes: [
        .init(key: .configuration, value: "Debug"),

        // This is considered top-level since it's not indented.
        .init(key: .flags, value: "-I /c/d -wmo")
      ],
      rules: [
        .init(
          inputs: ["xyz", "foo.swift"],
          outputs: ["foo.o"],
          dependencies: ["baz.o"],
          attributes: [
            .swiftModuleName: .init(key: .swiftModuleName, value: "foobar"),
            .flags: .init(key: .flags, value: "-I /a/b -wmo"),
          ]
        ),
        .init(
          inputs: ["CUSTOM_COMMAND", "baz.swift"],
          outputs: ["baz.o"],
          dependencies: [],
          attributes: [
            .command: .init(key: .command, value: "/bin/swiftc -I /a/b -wmo"),
          ]
        )
      ]
    )
  }

  func testEscape() throws {
    for newline in ["\n", "\r", "\r\n"] {
      assertParse("""
        build foo.o$:: xyz$ foo$$.swift | baz$ bar.o
          FLAGS = -I /a$\(newline)\
                  /b -wmo
          COMMAND = swiftc$$
        """, rules: [
          .init(
            inputs: ["xyz foo$.swift"],
            outputs: ["foo.o:"],
            dependencies: ["baz bar.o"],
            attributes: [
              .flags: .init(key: .flags, value: "-I /a/b -wmo"),
              .command: .init(key: .command, value: "swiftc$")
            ]
          )
        ]
      )
    }
  }
}
