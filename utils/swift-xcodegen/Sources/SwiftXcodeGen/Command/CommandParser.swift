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
  private var input: ByteScanner
  private var knownCommand: KnownCommand?

  private init(_ input: UnsafeBufferPointer<UInt8>) {
    self.input = ByteScanner(input)
  }

  static func parseCommand(_ input: String) throws -> Command {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      return try parser.parseCommand()
    }
  }

  static func parseKnownCommandOnly(_ input: String) throws -> Command? {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      guard let executable = try parser.consumeExecutable(
        dropPrefixBeforeKnownCommand: true
      ) else {
        return nil
      }
      return Command(executable: executable, args: try parser.consumeArguments())
    }
  }

  static func parseArguments(
    _ input: String, for command: KnownCommand
  ) throws -> [Command.Argument] {
    var input = input
    return try input.withUTF8 { bytes in
      var parser = Self(bytes)
      parser.knownCommand = command
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
      guard let executableUTF8 = try input.consumeElement() else {
        return nil
      }
      executable = AnyPath(String(utf8: executableUTF8))
      self.knownCommand = executable.knownCommand

      // If we want to drop the prefix before a known command, keep dropping
      // elements until we find the known command.
    } while dropPrefixBeforeKnownCommand && knownCommand == nil
    return executable
  }

  private mutating func consumeArguments() throws -> [Command.Argument] {
    var args = [Command.Argument]()
    while let arg = try consumeArgument() {
      args.append(arg)
    }
    return args
  }
}

enum CommandParserError: Error, CustomStringConvertible {
  case expectedCommand
  case unterminatedStringLiteral

  var description: String {
    switch self {
    case .expectedCommand:
      return "expected command in command line"
    case .unterminatedStringLiteral:
      return "unterminated string literal in command line"
    }
  }
}

fileprivate extension ByteScanner.Consumer {
  /// Consumes a character, unescaping if needed.
  mutating func consumeUnescaped() -> Bool {
    if peek == "\\" {
      skip()
    }
    return eat()
  }

  mutating func consumeStringLiteral() throws {
    assert(peek == "\"")
    skip()
    repeat {
      if peek == "\"" {
        skip()
        return
      }
    } while consumeUnescaped()
    throw CommandParserError.unterminatedStringLiteral
  }
}

fileprivate extension ByteScanner {
  mutating func consumeElement() throws -> Bytes? {
    // Eat any leading whitespace.
    skip(while: \.isSpaceOrTab)

    // If we're now at the end of the input, nothing can be parsed.
    guard hasInput else { return nil }

    // Consume the element, stopping at the first space.
    return try consume(using: { consumer in
      switch consumer.peek {
      case \.isSpaceOrTab:
        return false
      case "\"":
        try consumer.consumeStringLiteral()
        return true
      default:
        return consumer.consumeUnescaped()
      }
    })
  }
}

extension CommandParser {
  mutating func tryConsumeOption(
    _ option: ByteScanner, for flagSpec: Command.FlagSpec.Element
  ) throws -> Command.Argument? {
    var option = option
    let flag = flagSpec.flag
    guard option.tryEat(utf8: flag.name.rawValue) else {
      return nil
    }
    func makeOption(
      spacing: Command.OptionSpacing, _ value: String
    ) -> Command.Argument {
      .option(flag, spacing: spacing, value: value)
    }
    let spacing = flagSpec.spacing
    do {
      var option = option
      if spacing.contains(.equals), option.tryEat("="), option.hasInput {
        return makeOption(spacing: .equals, String(utf8: option.remaining))
      }
    }
    if spacing.contains(.unspaced), option.hasInput {
      return makeOption(spacing: .unspaced, String(utf8: option.remaining))
    }
    if spacing.contains(.spaced), !option.hasInput,
       let value = try input.consumeElement() {
      return makeOption(spacing: .spaced, String(utf8: value))
    }
    return option.empty ? .flag(flag) : nil
  }

  mutating func consumeOption(
    _ option: ByteScanner, dash: Command.Flag.Dash
  ) throws -> Command.Argument? {
    // NOTE: If we ever expand the list of flags, we'll likely want to use a
    // trie or something here.
    guard let knownCommand else { return nil }
    for spec in knownCommand.flagSpec.flags where spec.flag.dash == dash {
      if let option = try tryConsumeOption(option, for: spec) {
        return option
      }
    }
    return nil
  }

  mutating func consumeArgument() throws -> Command.Argument? {
    guard let element = try input.consumeElement() else { return nil }
    return try element.withUnsafeBytes { bytes in
      var option = ByteScanner(bytes)
      var numDashes = 0
      if option.tryEat("-") { numDashes += 1 }
      if option.tryEat("-") { numDashes += 1 }
      guard let dash = Command.Flag.Dash(numDashes: numDashes),
            let result = try consumeOption(option, dash: dash) else {
        return .value(String(utf8: option.whole))
      }
      return result
    }
  }
}
