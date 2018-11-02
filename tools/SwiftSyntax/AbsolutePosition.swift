//===--------------- AbsolutePosition.swift - Source Positions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An absolute position in a source file as text - the absolute utf8Offset from
/// the start, line, and column.
public final class AbsolutePosition {
  public fileprivate(set) var utf8Offset: Int
  public fileprivate(set) var line: Int
  public fileprivate(set) var column: Int

  public init(line: Int = 1, column: Int = 1, utf8Offset: Int = 0) {
    self.line = line
    self.column = column
    self.utf8Offset = utf8Offset
  }

  internal func add(columns: Int) {
    self.column += columns
    self.utf8Offset += columns
  }

  internal func add(lines: Int, size: Int) {
    self.line += lines * size
    self.column = 1
    self.utf8Offset += lines * size
  }

  /// Use some text as a reference for adding to the absolute position,
  /// taking note of newlines, etc.
  internal func add(text: String) {
    for char in text {
      switch char {
      case "\n", "\r\n":
        line += 1
        column = 1
      default:
        column += 1
      }

      // FIXME: This is currently very wasteful, but should be fast once the
      //        small-string optimization lands.
      utf8Offset += String(char).utf8.count
    }
  }

  internal func copy() -> AbsolutePosition {
    return AbsolutePosition(line: line, column: column,
      utf8Offset: utf8Offset)
  }
}
