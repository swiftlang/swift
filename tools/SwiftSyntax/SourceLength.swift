//===------------------ SourceLength.swift - Source Length ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The length a syntax node spans in the source code. From any AbsolutePosition
/// you reach a node's end location by either adding its UTF-8 length or by
/// inserting `lines` newlines and then moving `columns` columns to the right.
public struct SourceLength {
  public let newlines: Int
  public let columnsAtLastLine: Int
  public let utf8Length: Int

  /// Construct the source length of a given text
  public init(of text: String) {
    var newlines = 0
    var columnsAtLastLine = 0
    var utf8Length = 0
    for char in text {
      let charLength = String(char).utf8.count
      utf8Length += charLength
      switch char {
      case "\n", "\r\n", "\r":
        newlines += 1
        columnsAtLastLine = 0
      default:
        columnsAtLastLine += charLength
      }
    }
    self.newlines = newlines
    self.columnsAtLastLine = columnsAtLastLine
    self.utf8Length = utf8Length
  }

  public init(newlines: Int, columnsAtLastLine: Int, utf8Length: Int) {
    self.newlines = newlines
    self.columnsAtLastLine = columnsAtLastLine
    self.utf8Length = utf8Length
  }

  /// A zero-length source length
  public static let zero: SourceLength = 
      SourceLength(newlines: 0, columnsAtLastLine: 0, utf8Length: 0)

  /// Combine the length of two source length. Note that the addition is *not*
  /// commutative (3 columns + 1 line = 1 line but 1 line + 3 columns = 1 line
  /// and 3 columns)
  public static func +(lhs: SourceLength, rhs: SourceLength) -> SourceLength {
    let utf8Length = lhs.utf8Length + rhs.utf8Length
    let newlines = lhs.newlines + rhs.newlines
    let columnsAtLastLine: Int
    if rhs.newlines == 0 {
      columnsAtLastLine = lhs.columnsAtLastLine + rhs.columnsAtLastLine
    } else {
      columnsAtLastLine = rhs.columnsAtLastLine
    }
    return SourceLength(newlines: newlines, 
                        columnsAtLastLine: columnsAtLastLine, 
                        utf8Length: utf8Length)
  }

  public static func +=(lhs: inout SourceLength, rhs: SourceLength) {
    lhs = lhs + rhs
  }
}

extension AbsolutePosition {
  /// Determine the AbsolutePosition by advancing the `lhs` by the given source
  /// length.
  public static func +(lhs: AbsolutePosition, rhs: SourceLength) 
      -> AbsolutePosition {
    let utf8Offset = lhs.utf8Offset + rhs.utf8Length
    let line = lhs.line + rhs.newlines
    let column: Int
    if rhs.newlines == 0 {
      column = lhs.column + rhs.columnsAtLastLine
    } else {
      column = rhs.columnsAtLastLine + 1 // AbsolutePosition has 1-based columns
    }
    return AbsolutePosition(line: line, column: column, utf8Offset: utf8Offset)
  }

  public static func +=(lhs: inout AbsolutePosition, rhs: SourceLength) {
    lhs = lhs + rhs
  }
}
