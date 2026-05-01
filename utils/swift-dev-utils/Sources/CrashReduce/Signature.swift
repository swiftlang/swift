//===--- Signature.swift --------------------------------------------------===//
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

import Synchronization

public final class Signature: Sendable {
  let symbols: [String]
  let assertion: Assertion?

  private let _short = Mutex<ShortSignature??>(nil)
  public var short: ShortSignature? {
    _short.withLock { _short in
      if let _short {
        return _short
      }
      var result: ShortSignature?
      if let symbol {
        result = try? SymbolParser.parse(symbol).shortSignature
      } else if let assertion, let function = assertion.function {
        result = .init(symbol: function)
      }
      if result?.symbol.isEmpty == true {
        result = nil
      }
      _short = result
      return result
    }
  }

  init(symbols: [String], assertion: Assertion?) {
    self.symbols = symbols
    self.assertion = assertion
  }

  public static func assertion(_ assert: Assertion) -> Self {
    .init(symbols: [], assertion: assert)
  }

  public static func symbols(_ syms: [String], assert: Assertion?) -> Self {
    .init(symbols: syms, assertion: assert)
  }

  public var symbol: String? {
    symbols.first
  }

  public var isAssertion: Bool {
    assertion != nil && symbol == nil
  }

  static var unknown: Signature {
    Signature(symbols: ["unknown"], assertion: nil)
  }

  public var isUnknown: Bool {
    symbol == "unknown"
  }
}

extension Signature: Hashable {
  public static func == (lhs: Signature, rhs: Signature) -> Bool {
    lhs.symbols == rhs.symbols && lhs.assertion == rhs.assertion
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(symbols)
    if let assertion {
      hasher.combine(assertion)
    }
  }
}

extension Signature: Comparable {
  public static func < (lhs: Signature, rhs: Signature) -> Bool {
    if lhs.symbols != rhs.symbols {
      return lhs.symbols.lexicographicallyPrecedes(rhs.symbols)
    }
    return lhs.assertion?.fullMessage ?? "" < rhs.assertion?.fullMessage ?? ""
  }
}

extension Signature: Codable {
  enum CodingKeys: String, CodingKey {
    case symbols, assertion
  }
  public convenience init(from decoder: any Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    self.init(
      symbols: try container.decodeIfPresent([String].self, forKey: .symbols) ?? [],
      assertion:  try container.decodeIfPresent(Assertion.self, forKey: .assertion)
    )
  }

  public func encode(to encoder: any Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encodeIfPresent(symbols, forKey: .symbols)
    try container.encodeIfPresent(assertion, forKey: .assertion)
  }
}

extension Signature: CustomStringConvertible {
  public var description: String { symbol ?? assertion?.fullMessage ?? "" }
  public var shortDescription: String { short?.symbol ?? description }
}
