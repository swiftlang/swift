//===--- NinjaParser.swift ------------------------------------------------===//
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

import Foundation

struct NinjaParser {
  private var lexer: Lexer

  private init(_ input: UnsafeRawBufferPointer) throws {
    self.lexer = Lexer(ByteScanner(input))
  }

  static func parse(_ input: Data) throws -> NinjaBuildFile {
    try input.withUnsafeBytes { bytes in
      var parser = try Self(bytes)
      return try parser.parse()
    }
  }
}

fileprivate enum NinjaParseError: Error {
  case badAttribute
  case expected(NinjaParser.Lexeme)
}

fileprivate extension ByteScanner {
  mutating func consumeUnescaped(
    while pred: (Byte) -> Bool
  ) -> String? {
    let bytes = consume(using: { consumer in
      guard let c = consumer.peek, pred(c) else { return false }

      // Ninja uses '$' as the escape character.
      if c == "$" {
        switch consumer.peek(ahead: 1) {
        case let c? where c.isSpaceOrTab:
          fallthrough
        case "$", ":":
          // Skip the '$' and take the unescaped character.
          consumer.skip()
          return consumer.eat()
        case let c? where c.isNewline:
          // This is a line continuation, skip the newline, and strip any
          // following space.
          consumer.skip(untilAfter: \.isNewline)
          consumer.skip(while: \.isSpaceOrTab)
          return true
        default:
          // Unknown escape sequence, treat the '$' literally.
          break
        }
      }
      return consumer.eat()
    })
    return bytes.isEmpty ? nil : String(utf8: bytes)
  }
}

fileprivate extension NinjaParser {
  typealias BuildRule = NinjaBuildFile.BuildRule
  typealias Attribute = NinjaBuildFile.Attribute

  struct ParsedAttribute: Hashable {
    var key: String
    var value: String
  }

  enum Lexeme: Hashable {
    case attribute(ParsedAttribute)
    case element(String)
    case build
    case newline
    case colon
    case equal
    case pipe
    case doublePipe
  }

  struct Lexer {
    private var input: ByteScanner
    private(set) var lexeme: Lexeme?
    private(set) var isAtStartOfLine = true
    private(set) var leadingTriviaCount = 0

    init(_ input: ByteScanner) {
      self.input = input
      self.lexeme = lex()
    }
  }

  var peek: Lexeme? { lexer.lexeme }

  @discardableResult
  mutating func tryEat(_ lexeme: Lexeme) -> Bool {
    guard peek == lexeme else { return false }
    eat()
    return true
  }

  mutating func tryEatElement() -> String? {
    guard case .element(let str) = peek else { return nil }
    eat()
    return str
  }

  @discardableResult
  mutating func eat() -> Lexeme? {
    defer {
      lexer.eat()
    }
    return peek
  }
}

fileprivate extension Byte {
  var isNinjaOperator: Bool {
    switch self {
    case ":", "|", "=":
      true
    default:
      false
    }
  }
}

fileprivate extension NinjaBuildFile.Attribute {
  init?(_ parsed: NinjaParser.ParsedAttribute) {
    // Ignore unknown attributes for now.
    guard let key = Key(rawValue: parsed.key) else { return nil }
    self.init(key: key, value: parsed.value)
  }
}

extension NinjaParser.Lexer {
  typealias Lexeme = NinjaParser.Lexeme

  private mutating func consumeOperator() -> Lexeme {
    switch input.eat() {
    case ":":
      return .colon
    case "=":
      return .equal
    case "|":
      if input.tryEat("|") {
        return .doublePipe
      }
      return .pipe
    default:
      fatalError("Invalid operator character")
    }
  }

  private mutating func consumeElement() -> String? {
    input.consumeUnescaped(while: { char in
      switch char {
      case let c where c.isNinjaOperator || c.isSpaceTabOrNewline:
        false
      default:
        true
      }
    })
  }

  private mutating func tryConsumeAttribute(key: String) -> Lexeme? {
    input.tryEating { input in
      input.skip(while: \.isSpaceOrTab)
      guard input.tryEat("=") else { return nil }
      input.skip(while: \.isSpaceOrTab)
      guard let value = input.consumeUnescaped(while: { !$0.isNewline }) else {
        return nil
      }
      return .attribute(.init(key: key, value: value))
    }
  }

  private mutating func lex() -> Lexeme? {
    while true {
      isAtStartOfLine = input.previous?.isNewline ?? true
      leadingTriviaCount = input.eat(while: \.isSpaceOrTab)?.count ?? 0

      guard let c = input.peek else { return nil }
      if c == "#" {
        input.skip(untilAfter: \.isNewline)
        continue
      }
      if c.isNewline {
        input.skip(untilAfter: \.isNewline)
        if isAtStartOfLine {
          // Ignore empty lines, newlines are only semantically meaningful
          // when they delimit non-empty lines.
          continue
        }
        return .newline
      }
      if c.isNinjaOperator {
        return consumeOperator()
      }
      if isAtStartOfLine && input.tryEat(utf8: "build") {
        return .build
      }
      guard let element = consumeElement() else { return nil }

      // If we're on a newline, check to see if we can lex an attribute.
      if isAtStartOfLine {
        if let attr = tryConsumeAttribute(key: element) {
          return attr
        }
      }
      return .element(element)
    }
  }

  @discardableResult
  mutating func eat() -> Lexeme? {
    defer {
      lexeme = lex()
    }
    return lexeme
  }
}

fileprivate extension NinjaParser {
  mutating func skipLine() {
    while let lexeme = eat(), lexeme != .newline {}
  }

  mutating func parseAttribute() throws -> ParsedAttribute? {
    guard case let .attribute(attr) = peek else { return nil }
    eat()
    tryEat(.newline)
    return attr
  }

  mutating func parseBuildRule() throws -> BuildRule? {
    let indent = lexer.leadingTriviaCount
    guard tryEat(.build) else { return nil }

    var outputs: [String] = []
    while let str = tryEatElement() {
      outputs.append(str)
    }

    // Ignore implicit outputs for now.
    if tryEat(.pipe) {
      while tryEatElement() != nil {}
    }

    guard tryEat(.colon) else {
      throw NinjaParseError.expected(.colon)
    }

    var isPhony = false
    var inputs: [String] = []
    while let str = tryEatElement() {
      if str == "phony" {
        isPhony = true
      } else {
        inputs.append(str)
      }
    }

    if isPhony {
      skipLine()
      return .phony(for: outputs, inputs: inputs)
    }

    var dependencies: [String] = []
    while true {
      if let str = tryEatElement() {
        dependencies.append(str)
        continue
      }
      if tryEat(.pipe) || tryEat(.doublePipe) {
        // Currently we don't distinguish between implicit and explicit deps.
        continue
      }
      break
    }

    // We're done with the line, skip to the next.
    skipLine()

    var attributes: [Attribute.Key: Attribute] = [:]
    while indent < lexer.leadingTriviaCount, let attr = try parseAttribute() {
      if let attr = Attribute(attr) {
        attributes[attr.key] = attr
      }
    }

    return BuildRule(
      inputs: inputs, 
      outputs: outputs,
      dependencies: dependencies,
      attributes: attributes
    )
  }

  mutating func parse() throws -> NinjaBuildFile {
    var buildRules: [BuildRule] = []
    var attributes: [Attribute.Key: Attribute] = [:]
    while peek != nil {
      if let rule = try parseBuildRule() {
        buildRules.append(rule)
        continue
      }
      if let attr = try parseAttribute() {
        if let attr = Attribute(attr) {
          attributes[attr.key] = attr
        }
        continue
      }
      // Ignore unknown bits of syntax like 'include' for now.
      eat()
    }
    return NinjaBuildFile(attributes: attributes, buildRules: buildRules)
  }
}
