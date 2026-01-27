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
  let symbol: String?
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
      } else if let assertion {
        result = .init(symbol: assertion.message)
      }
      if result?.symbol.isEmpty == true {
        result = nil
      }
      _short = result
      return result
    }
  }

  init(symbol: String?, assertion: Assertion?) {
    self.symbol = symbol
    self.assertion = assertion
  }

  public static func assertion(_ assert: Assertion) -> Self {
    .init(symbol: nil, assertion: assert)
  }

  public static func symbol(_ sym: String, assert: Assertion?) -> Self {
    .init(symbol: sym, assertion: assert)
  }

  public var isAssertion: Bool {
    assertion != nil && symbol == nil
  }
}

extension Signature: Hashable {
  public static func == (lhs: Signature, rhs: Signature) -> Bool {
    lhs.symbol == rhs.symbol && lhs.assertion == rhs.assertion
  }
  public func hash(into hasher: inout Hasher) {
    if let symbol {
      hasher.combine(symbol)
    }
    if let assertion {
      hasher.combine(assertion)
    }
  }
}

extension Signature: Comparable {
  public static func < (lhs: Signature, rhs: Signature) -> Bool {
    (lhs.symbol ?? "", lhs.assertion?.fullMessage ?? "") <
      (rhs.symbol ?? "", rhs.assertion?.fullMessage ?? "")
  }
}

extension Signature: Codable {
  enum CodingKeys: String, CodingKey {
    case symbol, assertion
  }
  public convenience init(from decoder: any Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    self.init(
      symbol: try container.decodeIfPresent(String.self, forKey: .symbol),
      assertion:  try container.decodeIfPresent(Assertion.self, forKey: .assertion)
    )
  }

  public func encode(to encoder: any Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encodeIfPresent(symbol, forKey: .symbol)
    try container.encodeIfPresent(assertion, forKey: .assertion)
  }
}

extension Signature: CustomStringConvertible {
  public var description: String { symbol ?? assertion?.fullMessage ?? "" }
  public var shortDescription: String { short?.symbol ?? description }
}
