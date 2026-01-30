//===--- CommandArgumentParser.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct CommandArgumentParser {
  private var input: ByteScanner
  public var flagSpec: Command.FlagSpec?
  public var stopAtShellOperator: Bool

  public init(
    input: ByteScanner, flagSpec: Command.FlagSpec?, stopAtShellOperator: Bool
  ) {
    self.input = input
    self.flagSpec = flagSpec
    self.stopAtShellOperator = stopAtShellOperator
  }

  public static func parse(
    _ string: String, flagSpec: Command.FlagSpec?, stopAtShellOperator: Bool
  ) throws -> [Command.Argument] {
    try string.scanningUTF8 { input in
      var parser = Self(
        input: input, flagSpec: flagSpec,
        stopAtShellOperator: stopAtShellOperator
      )
      return try parser.consumeArguments()
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
    throw CommandArgumentParserError.unterminatedStringLiteral
  }
}

extension CommandArgumentParser {
  public mutating func consumeArguments() throws -> [Command.Argument] {
    var args = [Command.Argument]()
    while let arg = try consumeArgument() {
      args.append(arg)
    }
    return args
  }

  public mutating func consumeElement() throws -> ByteScanner.Bytes? {
    // Eat any leading whitespace.
    input.skip(while: \.isSpaceOrTab)

    // If we're now at the end of the input, nothing can be parsed.
    guard input.hasInput else { return nil }

    // Consume the element, stopping at the first space or shell operator.
    let start = input.cursor
    let elt = try input.consume(using: { consumer in
      guard let char = consumer.peek else { return false }
      if stopAtShellOperator {
        switch char {
        case "<", ">", "(", ")", "|", "&", ";":
          return false
        default:
          break
        }
      }
      switch char {
      case \.isSpaceOrTab:
        return false
      case "\"":
        try consumer.consumeStringLiteral()
        return true
      default:
        return consumer.consumeUnescaped()
      }
    })
    // Note that we may have an empty element while still moving the cursor
    // for e.g '-I ""', which is an option with an empty value.
    return start != input.cursor ? elt : nil
  }

  private mutating func tryConsumeOption(
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
       let value = try consumeElement() {
      return makeOption(spacing: .spaced, String(utf8: value))
    }
    return option.empty ? .flag(flag) : nil
  }

  public mutating func consumeOption(
    _ option: ByteScanner, dash: Command.Flag.Dash
  ) throws -> Command.Argument? {
    // NOTE: If we ever expand the list of flags, we'll likely want to use a
    // trie or something here.
    guard let flagSpec else { return nil }
    for spec in flagSpec.flags where spec.flag.dash == dash {
      if let option = try tryConsumeOption(option, for: spec) {
        return option
      }
    }
    return nil
  }

  public mutating func consumeArgument() throws -> Command.Argument? {
    guard let element = try consumeElement() else { return nil }
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

fileprivate enum CommandArgumentParserError: Error, CustomStringConvertible {
  case unterminatedStringLiteral

  var description: String {
    switch self {
    case .unterminatedStringLiteral:
      return "unterminated string literal in command line"
    }
  }
}
