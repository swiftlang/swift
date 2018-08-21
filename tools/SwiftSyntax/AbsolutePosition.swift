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
public struct AbsolutePosition {
  public let utf8Offset: Int
  public let line: Int
  public let column: Int

  static let startOfFile = AbsolutePosition(line: 1, column: 1, utf8Offset: 0)

  public init(line: Int, column: Int, utf8Offset: Int) {
    self.line = line
    self.column = column
    self.utf8Offset = utf8Offset
  }
}
