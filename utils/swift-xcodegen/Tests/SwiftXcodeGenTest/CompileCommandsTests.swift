//===--- CompileCommandsTests.swift ---------------------------------------===//
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

fileprivate func assertParse(
  _ str: String,
  executable: String? = nil,
  args: [Command.Argument],
  knownCommandOnly: Bool = false,
  file: StaticString = #file,
  line: UInt = #line
) {
  do {
    let command =
      try knownCommandOnly
      ? CommandParser.parseKnownCommandOnly(str)
      : CommandParser.parseCommand(str)
    guard let command else {
      XCTFail("Failed to parse command")
      return
    }
    if let executable {
      XCTAssertEqual(executable, command.executable.rawPath, file: file, line: line)
    }
    XCTAssertEqual(args, command.args, file: file, line: line)
  } catch {
    XCTFail("\(error)", file: file, line: line)
  }
}

class CompileCommandsTests: XCTestCase {
  func testClangCommandParse() {
    assertParse("x -a -b", executable: "x", args: [.value("-a"), .value("-b")])

    assertParse("x -D -I", executable: "x", args: [.value("-D"), .value("-I")])

    assertParse(
      "x y clang -DX -I",
      executable: "clang",
      args: [.option(.D, spacing: .unspaced, value: "X"), .flag(.I)],
      knownCommandOnly: true
    )

    assertParse(
      "x y x/y/clang -DX -I",
      executable: "x/y/clang",
      args: [.option(.D, spacing: .unspaced, value: "X"), .flag(.I)],
      knownCommandOnly: true
    )

    for op in ["&&", "||", ">", "<", ">>", ";", "(", ")"] {
      assertParse(
        "x y x/y/clang -DX -I \(op) ignored",
        executable: "x/y/clang",
        args: [.option(.D, spacing: .unspaced, value: "X"), .flag(.I)],
        knownCommandOnly: true
      )
      assertParse(
        "x y x/y/clang -DX -I x\(op) ignored",
        executable: "x/y/clang",
        args: [
          .option(.D, spacing: .unspaced, value: "X"),
          .option(.I, spacing: .spaced, value: "x"),
        ],
        knownCommandOnly: true
      )
    }

    assertParse(
      #"x/y/clang \< x\< "<""#,
      executable: "x/y/clang",
      args: [.value("<"), .value("x<"), .value("<")],
      knownCommandOnly: true
    )

    assertParse(
      "clang -DX -I",
      args: [.option(.D, spacing: .unspaced, value: "X"), .flag(.I)]
    )

    assertParse(
      "clang++ -D I",
      args: [
        .option(.D, spacing: .spaced, value: "I")
      ]
    )
    assertParse(
      "clang -DI",
      args: [
        .option(.D, spacing: .unspaced, value: "I")
      ]
    )
    assertParse(
      "clang -DIII",
      args: [
        .option(.D, spacing: .unspaced, value: "III")
      ]
    )
    assertParse(
      "clang -DIII I",
      args: [
        .option(.D, spacing: .unspaced, value: "III"), .value("I"),
      ]
    )

    assertParse(
      #"clang -D"III" I"#,
      args: [
        .option(.D, spacing: .unspaced, value: #"III"#), .value("I"),
      ]
    )

    assertParse(
      #"clang -D\"III\" -I"#,
      args: [
        .option(.D, spacing: .unspaced, value: #""III""#), .flag(.I),
      ]
    )

    assertParse(
      #"clang -D"a b" -I"#,
      args: [
        .option(.D, spacing: .unspaced, value: #"a b"#), .flag(.I),
      ]
    )

    assertParse(
      #"clang -Da\ b -I"#,
      args: [
        .option(.D, spacing: .unspaced, value: #"a b"#), .flag(.I),
      ]
    )

    assertParse(
      #"clang -I"III""#,
      args: [
        .option(.I, spacing: .unspaced, value: #"III"#)
      ]
    )

    assertParse(
      #"clang -I\"III\""#,
      args: [
        .option(.I, spacing: .unspaced, value: #""III""#)
      ]
    )

    assertParse(
      #"clang -I"a b""#,
      args: [
        .option(.I, spacing: .unspaced, value: #"a b"#)
      ]
    )

    assertParse(
      #"clang -Ia\ b"#,
      args: [
        .option(.I, spacing: .unspaced, value: #"a b"#)
      ]
    )

    assertParse(
      #"clang -I="III""#,
      args: [
        .option(.I, spacing: .equals, value: #"III"#)
      ]
    )

    assertParse(
      #"clang -I="#,
      args: [
        .option(.I, spacing: .unspaced, value: #"="#)
      ]
    )

    assertParse(
      #"clang -I=\"III\""#,
      args: [
        .option(.I, spacing: .equals, value: #""III""#)
      ]
    )

    assertParse(
      #"clang -I="a b""#,
      args: [
        .option(.I, spacing: .equals, value: #"a b"#)
      ]
    )

    assertParse(
      #"clang -I=a\ b"#,
      args: [
        .option(.I, spacing: .equals, value: #"a b"#)
      ]
    )

    assertParse(
      #"clang -Wnosomething"#,
      args: [
        .option(.W, spacing: .unspaced, value: #"nosomething"#)
      ]
    )

    assertParse(
      #"clang --I=a"#,
      args: [.value("--I=a")]
    )

    assertParse(
      #"clang --Da"#,
      args: [.value("--Da")]
    )

    assertParse(
      #"clang --Wa"#,
      args: [.value("--Wa")]
    )
  }

  func testSwiftCommandParse() {
    assertParse(
      #"swiftc -FX"#,
      args: [.option(.F, spacing: .unspaced, value: "X")]
    )
    assertParse(
      #"swiftc -F X"#,
      args: [.option(.F, spacing: .spaced, value: "X")]
    )
    assertParse(
      #"swiftc -F=X"#,
      args: [.option(.F, spacing: .equals, value: "X")]
    )
    assertParse(
      #"swiftc -Fsystem X"#,
      args: [.option(.Fsystem, spacing: .spaced, value: "X")]
    )
  }

  func testCommandEscape() {
    XCTAssertEqual(Command.Argument.flag(.I).printedArgs, ["-I"])
    XCTAssertEqual(Command.Argument.value("hello").printedArgs, ["hello"])
    XCTAssertEqual(Command.Argument.value("he llo").printedArgs, [#""he llo""#])
    XCTAssertEqual(Command.Argument.value(#""hello""#).printedArgs, [#"\"hello\""#])
    XCTAssertEqual(Command.Argument.value(#""he llo""#).printedArgs, [#""\"he llo\"""#])

    XCTAssertEqual(
      Command.Argument.option(
        .I,
        spacing: .unspaced,
        value: "he llo"
      ).printedArgs,
      [#"-I"he llo""#]
    )

    XCTAssertEqual(
      Command.Argument.option(
        .I,
        spacing: .spaced,
        value: "he llo"
      ).printedArgs,
      ["-I", #""he llo""#]
    )

    XCTAssertEqual(
      Command.Argument.option(
        .I,
        spacing: .unspaced,
        value: #""he llo""#
      ).printedArgs,
      [#"-I"\"he llo\"""#]
    )

    XCTAssertEqual(
      Command.Argument.option(
        .I,
        spacing: .spaced,
        value: #""he llo""#
      ).printedArgs,
      ["-I", #""\"he llo\"""#]
    )

    XCTAssertEqual(
      try CommandParser.parseCommand(#"swift \\ \ "#).printed,
      #"swift \\ " ""#
    )
    XCTAssertEqual(
      try CommandParser.parseCommand(#"swift "\\ ""#).printed,
      #"swift "\\ ""#
    )
  }

  func testEmptyArg() {
    // The empty string immediately after '-I' is effectively ignored.
    assertParse(
      #"swiftc -I"" """#,
      args: [
        .option(.I, spacing: .spaced, value: "")
      ]
    )

    assertParse(
      #"swiftc -I "" """#,
      args: [
        .option(.I, spacing: .spaced, value: ""),
        .value(""),
      ]
    )

    assertParse(
      #"swiftc   -I   ""   "" "#,
      args: [
        .option(.I, spacing: .spaced, value: ""),
        .value(""),
      ]
    )

    assertParse(
      #"swiftc   -I       "#,
      args: [
        .flag(.I)
      ]
    )
  }

  func testSpaceBeforeCommand() {
    assertParse("  swiftc  ", executable: "swiftc", args: [])
    assertParse(
      "\t\tswiftc\t\ta b\t",
      executable: "swiftc",
      args: [
        .value("a"),
        .value("b"),
      ]
    )
  }
}
