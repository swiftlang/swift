//===----------------------------------------------------------------------===//
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

/// A type that can represent a string as a collection of characters.
///
/// Unlike `StringProtocol`, no assumptions are made about the encoding or
/// type of the characters.
@available(SwiftStdlib 9999, *)
public protocol UncheckedStringProtocol
  : BidirectionalCollection, Equatable, Hashable, Comparable,
    CustomDebugStringConvertible
  where Iterator.Element: FixedWidthInteger,
    Index == Int,
    SubSequence: UncheckedStringProtocol
{
  /// Calls the given closure with a buffer of `Element`s,
  /// which are *not* necessarily NUL-terminated.
  func withCharacterData<R, E>(
    _ body: (Span<Element>) throws(E) -> R
  ) throws(E) -> R
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    if lhs.count != rhs.count {
      return false
    }

    return lhs.withCharacterData { lhsData in
      rhs.withCharacterData { rhsData in
        return lhsData._elementsEqual(to: rhsData)
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    withCharacterData {
      $0._hashContents(into: &hasher)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.withCharacterData { lhsData in
      rhs.withCharacterData { rhsData in
        let minCount = Swift.min(lhsData.count, rhsData.count)
        var i = 0
        while i < minCount {
          let l = lhsData[i]
          let r = rhsData[i]
          if l != r {
            return l < r
          }
          i += 1
        }
        return lhsData.count < rhsData.count
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedStringProtocol {
  public var debugDescription: String {
    return withCharacterData { data in
      var result = "\""
      var i = 0
      while i < data.count {
        let ch = data[i]
        if ch == 92 {
          result += "\\\\"
        } else if ch == 34 {
          result += "\\\""
        } else if ch >= 32 && ch <= 127 {
          result += String(Unicode.Scalar(UInt32(ch))!)
        } else {
          result += "\\x{"
          result += String(ch, radix: 16)
          result += "}"
        }
        i += 1
      }
      result += "\""
      return result
    }
  }
}
