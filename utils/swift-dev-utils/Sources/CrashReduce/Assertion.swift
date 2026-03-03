//===--- Assertion.swift --------------------------------------------------===//
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

import Foundation

/// An assertion message from a crash log.
public struct Assertion: Hashable, Sendable {
  public var fullMessage: String
  public var message: String

  private static func matchAssert(_ str: String) -> Assertion? {
    str.scanningUTF8 { scanner in
      while scanner.hasInput {
        let start = scanner.cursor
        guard scanner.tryEat(utf8: "Assertion fail") else {
          _ = scanner.eat()
          continue
        }
        scanner.skip(untilAfter: { $0 == ":" })
        scanner.skip(while: \.isSpaceOrTab)
        guard scanner.peek == "(", let msg = scanner.consumeMessage() else {
          return nil
        }
        scanner.skip(while: \.isSpaceOrTab)
        guard scanner.tryEat(",") else { return nil }
        scanner.skip(while: \.isSpaceOrTab)
        guard scanner.tryEat(utf8: "function") else { return nil }
        scanner.skip(while: \.isSpaceOrTab)
        scanner.skip(until: { $0 == "," || $0.isSpaceOrTab })
        let full = scanner.decodeUTF8(start ..< scanner.cursor)
        return Assertion(fullMessage: full, message: msg)
      }
      return nil
    }
  }

  private static func matchExact(_ str: String, _ message: String) -> String? {
    str.scanningUTF8 { scanner in
      while scanner.hasInput {
        if scanner.tryEat(utf8: message) {
          return message
        }
        _ = scanner.eat()
      }
      return nil
    }
  }

  public init?(from str: String) {
    if let match = Self.matchAssert(str) {
      self = match
    } else {
      return nil
    }
  }

  init(fullMessage: String, message: String) {
    self.fullMessage = fullMessage
    self.message = message
  }
}

extension Assertion: Codable {
  public init(from decoder: any Decoder) throws {
    let container = try decoder.singleValueContainer()
    guard let match = Self.matchAssert(try container.decode(String.self)) else {
      throw DecodingError.dataCorrupted(
        .init(codingPath: decoder.codingPath, debugDescription: "failed")
      )
    }
    self = match
  }
  public func encode(to encoder: any Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(fullMessage)
  }
}

fileprivate extension ByteScanner {
  mutating func consumeStringLiteral() -> Bool {
    guard tryEat("\"") else { return false }
    while let c = eat() {
      switch c {
      case "\"":
        return true
      case "\\":
        _ = eat()
        continue
      default:
        continue
      }
    }
    return false
  }

  mutating func consumeMessage() -> String? {
    guard tryEat("(") else { return nil }
    let start = cursor
    var depth = 1
    while let c = peek {
      switch c {
      case "(":
        depth += 1
      case ")":
        depth -= 1
        if depth == 0 {
          defer {
            _ = eat()
          }
          return decodeUTF8(start ..< cursor).trimmingCharacters(in: .whitespaces)
        }
      case "\"":
        guard consumeStringLiteral() else { return nil }
        continue
      default:
        break
      }
      _ = eat()
    }
    return nil
  }
}
