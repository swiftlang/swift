//===--- Byte.swift -------------------------------------------------------===//
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

public struct Byte: Hashable {
  public var rawValue: UInt8
  public init(_ rawValue: UInt8) {
    self.rawValue = rawValue
  }
}

extension Byte: ExpressibleByUnicodeScalarLiteral {
  public init(unicodeScalarLiteral value: UnicodeScalar) {
    self.init(UInt8(ascii: value))
  }
}

extension Byte: Comparable {
  public static func < (lhs: Self, rhs: Self) -> Bool {
    lhs.rawValue < rhs.rawValue
  }
}

public extension Byte {
  var isSpaceOrTab: Bool {
    self == " " || self == "\t"
  }
  var isNewline: Bool {
    self == "\n" || self == "\r"
  }
  var isSpaceTabOrNewline: Bool {
    isSpaceOrTab || isNewline
  }
  var isDigit: Bool {
    switch self {
    case "0"..."9":
      true
    default:
      false
    }
  }
  var isHexDigit: Bool {
    switch self {
    case "a"..."f", "A"..."F", \.isDigit:
      true
    default:
      false
    }
  }
  var isWordChar: Bool {
    switch self {
    case "a"..."z", "A"..."Z", "0"..."9", "_":
      true
    default:
      false
    }
  }
  var lower: Byte {
    switch self {
    case "A"..."Z":
      Byte(rawValue + 32)
    default:
      self
    }
  }
}
