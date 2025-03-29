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
  private let filePath: AbsolutePath
  private let fileReader: (AbsolutePath) throws -> Data
  private var lexer: Lexer

  private init(input: UnsafeRawBufferPointer, filePath: AbsolutePath, fileReader: @escaping (AbsolutePath) throws -> Data) throws {
    self.filePath = filePath
    self.fileReader = fileReader
    self.lexer = Lexer(ByteScanner(input))
  }

  static func parse(filePath: AbsolutePath, fileReader: @escaping (AbsolutePath) throws -> Data = { try $0.read() }) throws -> NinjaBuildFile {

    try fileReader(filePath).withUnsafeBytes { bytes in
      var parser = try Self(input: bytes, filePath: filePath, fileReader: fileReader)
      return try parser.parse()
    }
  }
}

fileprivate enum NinjaParseError: Error {
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
        case "$", ":", \.isSpaceOrTab:
          // Skip the '$' and take the unescaped character.
          consumer.skip()
          return consumer.eat()
        case \.isNewline:
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
  typealias Rule = NinjaBuildFile.Rule
  typealias BuildEdge = NinjaBuildFile.BuildEdge

  struct ParsedBinding: Hashable {
    var key: String
    var value: String
  }

  enum Lexeme: Hashable {
    case binding(ParsedBinding)
    case element(String)
    case rule
    case build
    case include
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
      case \.isNinjaOperator, \.isSpaceTabOrNewline:
        false
      default:
        true
      }
    })
  }

  private mutating func tryConsumeBinding(key: String) -> Lexeme? {
    input.tryEating { input in
      input.skip(while: \.isSpaceOrTab)
      guard input.tryEat("=") else { return nil }
      input.skip(while: \.isSpaceOrTab)
      guard let value = input.consumeUnescaped(while: { !$0.isNewline }) else {
        return nil
      }
      return .binding(.init(key: key, value: value))
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
      if isAtStartOfLine {
        // decl keywords.
        if input.tryEat(utf8: "build") {
          return .build
        }
        if input.tryEat(utf8: "rule") {
          return .rule
        }
        if input.tryEat(utf8: "include") {
          return .include
        }
      }
      guard let element = consumeElement() else { return nil }

      // If we're on a newline, check to see if we can lex a binding.
      if isAtStartOfLine {
        if let binding = tryConsumeBinding(key: element) {
          return binding
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

  mutating func parseBinding() throws -> ParsedBinding? {
    guard case let .binding(binding) = peek else { return nil }
    eat()
    tryEat(.newline)
    return binding
  }

  /// ```
  /// rule rulename
  ///   command = ...
  ///   var = ...
  /// ```
  mutating func parseRule() throws -> Rule? {
    let indent = lexer.leadingTriviaCount
    guard tryEat(.rule) else { return nil }

    guard let ruleName = tryEatElement() else {
      throw NinjaParseError.expected(.element("<rule name>"))
    }
    guard tryEat(.newline) else {
      throw NinjaParseError.expected(.newline)
    }

    var bindings: [String: String] = [:]
    while indent < lexer.leadingTriviaCount, let binding = try parseBinding() {
      bindings[binding.key] = binding.value
    }

    return Rule(name: ruleName, bindings: bindings)
  }

  /// ```
  /// build out1... | implicit-out... : rulename input... | dep... || order-only-dep...
  ///   var = ...
  /// ```
  mutating func parseBuildEdge() throws -> BuildEdge? {
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

    guard let ruleName = tryEatElement() else {
      throw NinjaParseError.expected(.element("<rule name>"))
    }

    var inputs: [String] = []
    while let str = tryEatElement() {
      inputs.append(str)
    }

    if ruleName == "phony" {
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
        // Currently we don't distinguish between implicit deps and order-only deps.
        continue
      }
      break
    }

    // We're done with the line, skip to the next.
    skipLine()

    var bindings: [String: String] = [:]
    while indent < lexer.leadingTriviaCount, let binding = try parseBinding() {
      bindings[binding.key] = binding.value
    }

    return BuildEdge(
      ruleName: ruleName,
      inputs: inputs,
      outputs: outputs,
      dependencies: dependencies,
      bindings: bindings
    )
  }

  /// ```
  /// include path/to/sub.ninja
  /// ```
  mutating func parseInclude() throws -> NinjaBuildFile? {
    guard tryEat(.include) else { return nil }

    guard let fileName = tryEatElement() else {
      throw NinjaParseError.expected(.element("<path>"))
    }

    let baseDirectory = self.filePath.parentDir!
    let path = AnyPath(fileName).absolute(in: baseDirectory)
    return try NinjaParser.parse(filePath: path, fileReader: fileReader)
  }

  mutating func parse() throws -> NinjaBuildFile {
    var bindings: [String: String] = [:]
    var rules: [String: Rule] = [:]
    var buildEdges: [BuildEdge] = []
    while peek != nil {
      if let rule = try parseRule() {
        rules[rule.name] = rule
        continue
      }
      if let edge = try parseBuildEdge() {
        buildEdges.append(edge)
        continue
      }
      if let binding = try parseBinding() {
        bindings[binding.key] = binding.value
        continue
      }
      if let included = try parseInclude() {
        bindings.merge(included.bindings.values, uniquingKeysWith: { _, other in other })
        rules.merge(included.rules, uniquingKeysWith: { _, other in other })
        buildEdges.append(contentsOf: included.buildEdges)
        continue
      }
      // Ignore unknown bits of syntax like 'subninja' for now.
      eat()
    }
    return NinjaBuildFile(bindings: bindings, rules: rules, buildEdges: buildEdges)
  }
}
