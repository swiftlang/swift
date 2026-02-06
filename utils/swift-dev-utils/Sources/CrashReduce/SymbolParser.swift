//===--- SymbolParser.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct ShortSignature {
  public var namespace: String?
  public var symbol: String

  init(namespace: String? = nil, symbol: String) {
    self.namespace = namespace
    self.symbol = symbol
  }
}

extension ShortSignature: CustomStringConvertible {
  public var description: String {
    symbol
  }
}

extension String {
  fileprivate var containsVisitOrMatch: Bool {
    scanningUTF8 { scanner in
      while scanner.hasInput {
        if scanner.tryEat(utf8: "visit", caseSensitive: false) {
          return true
        }
        if scanner.tryEat(utf8: "match", caseSensitive: false) {
          return true
        }
        _ = scanner.eat()
      }
      return false
    }
  }
}

indirect enum Symbol {
  case atom(String) // x
  case generic(Symbol, args: [Symbol]) // x<y, z>
  case namespace([Symbol]) // x::y
  case concatenation([Symbol]) // x y

  /// Compute the 'short signature' for a parsed symbol. This is the part of the
  /// symbol that easy to identify for a crash.
  var shortSignature: ShortSignature {
    switch self {
    case .atom(let str):
      return .init(symbol: str)
    case .concatenation(let elts):
      // We can have space separated components for things like function types,
      // just take the longest component.
      return elts.map(\.shortSignature)
        .filter { $0.symbol != "decltype" }
        .max(by: { $0.symbol.count < $1.symbol.count }) ?? elts.last!.shortSignature
    case .generic(let base, _):
      // Generally arguments aren't interesting.
      return base.shortSignature
    case .namespace(let components):
      var components = components[...]
      // Drop uninteresting suffix components.
      while let component = components.last,
            case .atom = component, case let short = component.shortSignature,
            short.symbol == "operator" || short.symbol.first == "$" {
        components = components.dropLast()
      }
      // Drop uninteresting prefix components.
      var namespace: String?
      while components.count >= 2,
            let component = components.first,
            case .atom = component,
            let firstChar = component.shortSignature.symbol.first,
            !firstChar.isUppercase {
        if namespace == nil {
          namespace = component.shortSignature.symbol
        }
        components = components.dropFirst()
      }
      // Recognise the pattern '__(func|bind)<...>'.
      for component in components {
        if case .generic(let base, let args) = component,
           let firstArg = args.first,
           case let baseShortSig = base.shortSignature.symbol,
           baseShortSig == "__func" || baseShortSig == "__bind" {
          return firstArg.shortSignature
        }
      }
      if components.count >= 2 {
        let suffix = Array(components.suffix(2))
        // Recognize the pattern 'SomeVisitor<...>::visit', the interesting
        // signature is in the first generic argument.
        if case .generic(let base, let args) = suffix[0],
           let firstArg = args.first,
           base.shortSignature.symbol.containsVisitOrMatch,
           suffix[1].shortSignature.symbol.containsVisitOrMatch {
          return firstArg.shortSignature
        }
        // Recognise the pattern 'function_ref<...>::callback_fn<...>',
        // the interesting signature is the first argument of 'callback_fn'.
        if case .generic(let baseLHS, _) = suffix[0],
           case .generic(let baseRHS, let args) = suffix[1],
           let firstArg = args.first,
           baseLHS.shortSignature.symbol == "function_ref",
           baseRHS.shortSignature.symbol == "callback_fn" {
          return firstArg.shortSignature
        }
      }
      let sym = components
        .map(\.shortSignature.symbol)
        .filter { $0.first?.isLetter == true }
        .joined(separator: "::")
      return ShortSignature(namespace: namespace, symbol: sym)
    }
  }
}

/// A parser for a C++ symbol, this isn't meant to be entirely 100% accurate,
/// just "good enough" for extracting short signatures.
enum SymbolParser {
  static func parse(_ sym: String) throws -> Symbol {
    return try sym.scanningUTF8 { scanner in
      try scanner.parse()
    }
  }
}

fileprivate extension Byte {
  var isMetaChar: Bool {
    switch self {
    case "<", ">", ":", "(", ")", ",", " ", "\t":
      return true
    default:
      return false
    }
  }
}

fileprivate extension ByteScanner {
  enum ParseError: Error {
    case unterminated(Character)
    case unexpectedToken
    case expected(Character)
  }

  @discardableResult
  mutating func skipParen() throws -> Bool {
    guard tryEat("(") else { return false }
    var depth = 1
    while let c = eat() {
      switch c {
      case "(":
        depth += 1
      case ")":
        depth -= 1
        if depth == 0 {
          return true
        }
      default:
        continue
      }
    }
    throw ParseError.unterminated("(")
  }

  mutating func parseAtom() throws -> String? {
    let start = cursor
    // Skip things like '(anonymous namespace)' for simplicity.
    try skipParen()
    skip(until: \.isMetaChar)
    return cursor != start ? decodeUTF8(start ..< cursor) : nil
  }

  mutating func parseGenericArgs() throws -> [Symbol]? {
    guard tryEat("<") else { return nil }
    if tryEat(">") { return [] }

    var symbols: [Symbol] = []
    while hasInput {
      symbols.append(try parse())
      skip(until: { $0 == ">" || $0 == "," })
      if !hasInput || peek == ">" {
        break
      }
      guard tryEat(",") else {
        throw ParseError.expected(",")
      }
      skip(while: \.isSpaceOrTab)
    }

    guard tryEat(">") else {
      throw ParseError.unterminated("<")
    }

    return symbols
  }

  mutating func parseSimple() throws -> Symbol? {
    if let atom = try parseAtom() {
      var symbol = Symbol.atom(atom)

      if let args = try parseGenericArgs() {
        symbol = .generic(symbol, args: args)
      }

      // Postfix paren is a function, we don't care about the params.
      if try skipParen() {
        // We can have still have postfix junk after a function, skip it.
        skip(until: \.isMetaChar)
      }

      return symbol
    }
    guard !hasInput else {
      throw ParseError.unexpectedToken
    }
    return nil
  }

  mutating func parseNamespaced() throws -> Symbol? {
    guard let simple = try parseSimple() else { return nil }

    var components = [simple]
    while tryEat(utf8: "::") {
      guard let next = try parseSimple() else { break }
      components.append(next)
    }
    return components.count == 1 ? simple : .namespace(components)
  }

  mutating func parseConcatenation() throws -> Symbol? {
    guard let elt = try parseNamespaced() else { return nil }

    var elts = [elt]
    while tryEat(where: \.isSpaceOrTab) {
      skip(while: \.isSpaceOrTab)
      guard let next = try parseNamespaced() else { break }
      elts.append(next)
    }
    return elts.count == 1 ? elt : .concatenation(elts)
  }

  mutating func parse() throws -> Symbol {
    guard let compound = try parseConcatenation() else {
      throw ParseError.unexpectedToken
    }
    return compound
  }
}
