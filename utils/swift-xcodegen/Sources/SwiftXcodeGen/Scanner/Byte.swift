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

// Please forgive me...
func == (lhs: UnicodeScalar, rhs: Byte?) -> Bool {
  guard let rhs else { return false }
  return lhs.value == rhs.rawValue
}
func == (lhs: Byte?, rhs: UnicodeScalar) -> Bool {
  rhs == lhs
}
func != (lhs: UnicodeScalar, rhs: Byte?) -> Bool {
  !(lhs == rhs)
}
func != (lhs: Byte?, rhs: UnicodeScalar) -> Bool {
  rhs != lhs
}

func ~= (pattern: UnicodeScalar, match: Byte) -> Bool {
  pattern == match
}
func ~= (pattern: UnicodeScalar, match: Byte?) -> Bool {
  pattern == match
}

extension Byte? {
  var isSpaceOrTab: Bool {
    self?.isSpaceOrTab == true
  }
  var isNewline: Bool {
    self?.isNewline == true
  }
  var isSpaceTabOrNewline: Bool {
    self?.isSpaceTabOrNewline == true
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
  init(ascii scalar: UnicodeScalar) {
    assert(scalar.isASCII)
    self.rawValue = UInt8(scalar.value)
  }
  var scalar: UnicodeScalar {
    UnicodeScalar(UInt32(rawValue))!
  }
  var char: Character {
    .init(scalar)
  }
}
