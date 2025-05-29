//===--- StringParser.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A simple utility to parse a string.
public struct StringParser {
  private var s: Substring
  private let originalLength: Int
  
  private mutating func consumeWhitespace() {
    s = s.drop { $0.isWhitespace }
  }

  public init(_ string: String) {
    s = Substring(string)
    originalLength = string.count
  }
  
  public mutating func isEmpty() -> Bool {
    consumeWhitespace()
    return s.isEmpty
  }

  public mutating func consume(_ str: String) -> Bool {
    consumeWhitespace()
    if !s.starts(with: str) { return false }
    s = s.dropFirst(str.count)
    return true
  }

  public mutating func consumeInt(withWhiteSpace: Bool = true) -> Int? {
    if withWhiteSpace {
      consumeWhitespace()
    }
    var intStr = ""
    s = s.drop {
      if $0.isNumber {
        intStr.append($0)
        return true
      }
      return false
    }
    return Int(intStr)
  }
  
  public mutating func consumeIdentifier() -> String? {
    consumeWhitespace()
    var name = ""
    s = s.drop {
      if $0.isLetter {
        name.append($0)
        return true
      }
      return false
    }
    return name.isEmpty ? nil : name
  }
  
  public func throwError(_ message: StaticString) throws -> Never {
    throw ParsingError(message: message, position: originalLength - s.count)
  }
}

public struct ParsingError : Error {
  public let message: StaticString
  public let position: Int
}
