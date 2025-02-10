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

struct Byte: Hashable {
  var rawValue: UInt8
  init(_ rawValue: UInt8) {
    self.rawValue = rawValue
  }
}

extension Byte: ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: UnicodeScalar) {
    self.init(UInt8(ascii: value))
  }
}

extension Byte: Comparable {
  static func < (lhs: Self, rhs: Self) -> Bool {
    lhs.rawValue < rhs.rawValue
  }
}

extension Byte {
  var isSpaceOrTab: Bool {
    self == " " || self == "\t"
  }
  var isNewline: Bool {
    self == "\n" || self == "\r"
  }
  var isSpaceTabOrNewline: Bool {
    isSpaceOrTab || isNewline
  }
}
