//===--- CommandParser.swift ----------------------------------------------===//
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

struct CommandParser {
  private var argParser: CommandArgumentParser

  private init(_ input: UnsafeBufferPointer<UInt8>) {
    self.argParser = CommandArgumentParser(
      input: ByteScanner(input), flagSpec: nil, stopAtShellOperator: false
    )
  }

  static func parseCommand(_ input: String) throws -> Command {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      return try parser.parseCommand()
    }
  }

  /// Parse an arbitrary shell command, returning the first single invocation
  /// of a known command.
  static func parseKnownCommandOnly(_ input: String) throws -> Command? {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      guard let executable = try parser.consumeExecutable(
        dropPrefixBeforeKnownCommand: true
      ) else {
        return nil
      }
      // We're parsing an arbitrary shell command so stop if we hit a shell
      // operator like '&&'
      parser.argParser.stopAtShellOperator = true
      return Command(
        executable: executable, args: try parser.consumeArguments()
      )
    }
  }

  static func parseArguments(
    _ input: String, for command: KnownCommand
  ) throws -> [Command.Argument] {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      parser.argParser.flagSpec = command.flagSpec
      return try parser.consumeArguments()
    }
  }

  private mutating func parseCommand() throws -> Command {
    guard let executable = try consumeExecutable() else {
      throw CommandParserError.expectedCommand
    }
    return Command(executable: executable, args: try consumeArguments())
  }

  private mutating func consumeExecutable(
    dropPrefixBeforeKnownCommand: Bool = false
  ) throws -> AnyPath? {
    var executable: AnyPath
    repeat {
      guard let executableUTF8 = try argParser.consumeElement() else {
        return nil
      }
      executable = AnyPath(String(utf8: executableUTF8))
      argParser.flagSpec = executable.knownCommand?.flagSpec

      // If we want to drop the prefix before a known command, keep dropping
      // elements until we find the known command.
    } while dropPrefixBeforeKnownCommand && argParser.flagSpec == nil
    return executable
  }

  private mutating func consumeArguments() throws -> [Command.Argument] {
    try argParser.consumeArguments()
  }
}

fileprivate enum CommandParserError: Error, CustomStringConvertible {
  case expectedCommand

  var description: String {
    switch self {
    case .expectedCommand:
      return "expected command in command line"
    }
  }
}
