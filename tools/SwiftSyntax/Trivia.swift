//===------------------- Trivia.swift - Source Trivia Enum ----------------===//
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

import Foundation

/// A contiguous stretch of a single kind of trivia. The constituent part of
/// a `Trivia` collection.
///
/// For example, four spaces would be represented by
/// `.spaces(4)`
///
/// In general, you should deal with the actual Trivia collection instead
/// of individual pieces whenever possible.
public enum TriviaPiece: Codable {
  enum CodingKeys: CodingKey {
    case kind, value
  }
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let kind = try container.decode(String.self, forKey: .kind)
    switch kind {
    case "Space":
      let value = try container.decode(Int.self, forKey: .value)
      self = .spaces(value)
    case "Tab":
      let value = try container.decode(Int.self, forKey: .value)
      self = .tabs(value)
    case "VerticalTab":
      let value = try container.decode(Int.self, forKey: .value)
      self = .verticalTabs(value)
    case "Formfeed":
      let value = try container.decode(Int.self, forKey: .value)
      self = .formfeeds(value)
    case "Newline":
      let value = try container.decode(Int.self, forKey: .value)
      self = .newlines(value)
    case "CarriageReturn":
      let value = try container.decode(Int.self, forKey: .value)
      self = .carriageReturns(value)
    case "CarriageReturnLineFeed":
      let value = try container.decode(Int.self, forKey: .value)
      self = .carriageReturnLineFeeds(value)
    case "Backtick":
      let value = try container.decode(Int.self, forKey: .value)
      self = .backticks(value)
    case "LineComment":
      let value = try container.decode(String.self, forKey: .value)
      self = .lineComment(value)
    case "BlockComment":
      let value = try container.decode(String.self, forKey: .value)
      self = .blockComment(value)
    case "DocLineComment":
      let value = try container.decode(String.self, forKey: .value)
      self = .docLineComment(value)
    case "DocBlockComment":
      let value = try container.decode(String.self, forKey: .value)
      self = .docLineComment(value)
    case "GarbageText":
      let value = try container.decode(String.self, forKey: .value)
      self = .garbageText(value)
    default:
      let context =
        DecodingError.Context(codingPath: [CodingKeys.kind],
                              debugDescription: "invalid TriviaPiece kind \(kind)")
      throw DecodingError.valueNotFound(String.self, context)
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    switch self {
    case .blockComment(let comment):
      try container.encode("BlockComment", forKey: .kind)
      try container.encode(comment, forKey: .value)
    case .docBlockComment(let comment):
      try container.encode("DocBlockComment", forKey: .kind)
      try container.encode(comment, forKey: .value)
    case .docLineComment(let comment):
      try container.encode("DocLineComment", forKey: .kind)
      try container.encode(comment, forKey: .value)
    case .lineComment(let comment):
      try container.encode("LineComment", forKey: .kind)
      try container.encode(comment, forKey: .value)
    case .garbageText(let text):
      try container.encode("GarbageText", forKey: .kind)
      try container.encode(text, forKey: .value)
    case .formfeeds(let count):
      try container.encode("Formfeed", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .backticks(let count):
      try container.encode("Backtick", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .newlines(let count):
      try container.encode("Newline", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .carriageReturns(let count):
      try container.encode("CarriageReturn", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .carriageReturnLineFeeds(let count):
      try container.encode("CarriageReturnLineFeeds", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .spaces(let count):
      try container.encode("Space", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .tabs(let count):
      try container.encode("Tab", forKey: .kind)
      try container.encode(count, forKey: .value)
    case .verticalTabs(let count):
      try container.encode("VerticalTab", forKey: .kind)
      try container.encode(count, forKey: .value)

    }
  }

  /// A space ' ' character.
  case spaces(Int)

  /// A tab '\t' character.
  case tabs(Int)

  /// A vertical tab '\v' character.
  case verticalTabs(Int)

  /// A form-feed '\f' character.
  case formfeeds(Int)

  /// A newline '\n' character.
  case newlines(Int)

  /// A carriage-return '\r' character.
  case carriageReturns(Int)
  
  /// A newline two bytes sequence consists of '\r' and '\n' characters.
  case carriageReturnLineFeeds(Int)

  /// A backtick '`' character, used to escape identifiers.
  case backticks(Int)

  /// A developer line comment, starting with '//'
  case lineComment(String)

  /// A developer block comment, starting with '/*' and ending with '*/'.
  case blockComment(String)

  /// A documentation line comment, starting with '///'.
  case docLineComment(String)

  /// A documentation block comment, starting with '/**' and ending with '*/.
  case docBlockComment(String)

  /// Any skipped text.
  case garbageText(String)
}

extension TriviaPiece: TextOutputStreamable {
  /// Prints the provided trivia as they would be written in a source file.
  ///
  /// - Parameter stream: The stream to which to print the trivia.
  public func write<Target>(to target: inout Target)
    where Target: TextOutputStream {
    func printRepeated(_ character: String, count: Int) {
      for _ in 0..<count { target.write(character) }
    }
    switch self {
    case let .spaces(count): printRepeated(" ", count: count)
    case let .tabs(count): printRepeated("\t", count: count)
    case let .verticalTabs(count): printRepeated("\u{2B7F}", count: count)
    case let .formfeeds(count): printRepeated("\u{240C}", count: count)
    case let .newlines(count): printRepeated("\n", count: count)
    case let .carriageReturns(count): printRepeated("\r", count: count)
    case let .carriageReturnLineFeeds(count): printRepeated("\r\n", count: count)
    case let .backticks(count): printRepeated("`", count: count)
    case let .lineComment(text),
         let .blockComment(text),
         let .docLineComment(text),
         let .docBlockComment(text),
         let .garbageText(text):
      target.write(text)
    }
  }

  /// Computes the information from this trivia to inform the source locations
  /// of the associated tokens.
  /// Specifically, walks through the trivia and keeps track of every newline
  /// to give a number of how many newlines and UTF8 characters appear in the
  /// trivia, along with the UTF8 offset of the last column.
  func characterSizes() -> (lines: Int, lastColumn: Int, utf8Length: Int) {
    switch self {
    case .spaces(let n),
         .tabs(let n),
         .verticalTabs(let n),
         .formfeeds(let n),
         .backticks(let n):
      return (lines: 0, lastColumn: n, utf8Length: n)
    case .newlines(let n):
      return (lines: n, lastColumn: 0, utf8Length: n)
    case .carriageReturns(let n):
      return (lines: n, lastColumn: 0, utf8Length: n)
    case .carriageReturnLineFeeds(let n):
      return (lines: n, lastColumn: 0, utf8Length: n * 2)
    case .lineComment(let text),
         .docLineComment(let text):
      let length = text.utf8.count
      return (lines: 0, lastColumn: length, utf8Length: length)
    case .blockComment(let text),
         .docBlockComment(let text),
         .garbageText(let text):
      var lines = 0
      var col = 0
      var total = 0
      var prevChar: UInt8? = nil
      // TODO: CR + LF should be regarded as one newline
      for char in text.utf8 {
        total += 1
        switch char {
        case 0x0a:
          if prevChar == 0x0d {
            /* ASCII CR LF */
            assert(col == 0)
          } else {
            /* ASCII newline */
            col = 0
            lines += 1
          }
        /* ASCII carriage-return */
        case 0x0d:
          col = 0
          lines += 1
        
        default:
          col += 1
        }
        prevChar = char
      }
      return (lines: lines, lastColumn: col, utf8Length: total)
    }
  }
}

/// A collection of leading or trailing trivia. This is the main data structure
/// for thinking about trivia.
public struct Trivia: Codable {
  let pieces: [TriviaPiece]

  /// Creates Trivia with the provided underlying pieces.
  public init(pieces: [TriviaPiece]) {
    self.pieces = pieces
  }

  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    var pieces = [TriviaPiece]()
    while let piece = try container.decodeIfPresent(TriviaPiece.self) {
      pieces.append(piece)
    }
    self.pieces = pieces
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    for piece in pieces {
      try container.encode(piece)
    }
  }

  /// Creates Trivia with no pieces.
  public static var zero: Trivia {
    return Trivia(pieces: [])
  }

  /// Creates a new `Trivia` by appending the provided `TriviaPiece` to the end.
  public func appending(_ piece: TriviaPiece) -> Trivia {
    var copy = pieces
    copy.append(piece)
    return Trivia(pieces: copy)
  }

  /// Return a piece of trivia for some number of space characters in a row.
  public static func spaces(_ count: Int) -> Trivia {
    return [.spaces(count)]
  }

  /// Return a piece of trivia for some number of tab characters in a row.
  public static func tabs(_ count: Int) -> Trivia {
    return [.tabs(count)]
  }

  /// A vertical tab '\v' character.
  public static func verticalTabs(_ count: Int) -> Trivia {
    return [.verticalTabs(count)]
  }

  /// A form-feed '\f' character.
  public static func formfeeds(_ count: Int) -> Trivia {
    return [.formfeeds(count)]
  }

  /// Return a piece of trivia for some number of newline characters
  /// in a row.
  public static func newlines(_ count: Int) -> Trivia {
    return [.newlines(count)]
  }

  /// Return a piece of trivia for some number of carriage-return characters
  /// in a row.
  public static func carriageReturns(_ count: Int) -> Trivia {
    return [.carriageReturns(count)]
  }

  /// Return a piece of trivia for some number of two bytes sequence
  /// consists of CR and LF in a row.
  public static func carriageReturnLineFeeds(_ count: Int) -> Trivia {
    return [.carriageReturnLineFeeds(count)]
  }

  /// Return a piece of trivia for some number of backtick '`' characters
  /// in a row.
  public static func backticks(_ count: Int) -> Trivia {
    return [.backticks(count)]
  }

  /// Return a piece of trivia for a single line of ('//') developer comment.
  public static func lineComment(_ text: String) -> Trivia {
    return [.lineComment(text)]
  }

  /// Return a piece of trivia for a block comment ('/* ... */')
  public static func blockComment(_ text: String) -> Trivia {
    return [.blockComment(text)]
  }

  /// Return a piece of trivia for a single line of ('///') doc comment.
  public static func docLineComment(_ text: String) -> Trivia {
    return [.docLineComment(text)]
  }

  /// Return a piece of trivia for a documentation block comment ('/** ... */')
  public static func docBlockComment(_ text: String) -> Trivia {
    return [.docBlockComment(text)]
  }

  /// Return a piece of trivia for any garbage text.
  public static func garbageText(_ text: String) -> Trivia {
    return [.garbageText(text)]
  }

  /// Computes the total sizes and offsets of all pieces in this Trivia.
  func characterSizes() -> (lines: Int, lastColumn: Int, utf8Length: Int) {
    var lines = 0
    var lastColumn = 0
    var length = 0
    for piece in pieces {
      let (ln, col, len) = piece.characterSizes()
      lines += ln
      lastColumn = col
      length += len
    }
    return (lines: lines, lastColumn: lastColumn, utf8Length: length)
  }
}

/// Conformance for Trivia to the Collection protocol.
extension Trivia: Collection {
  public var startIndex: Int {
    return pieces.startIndex
  }

  public var endIndex: Int {
    return pieces.endIndex
  }

  public func index(after i: Int) -> Int {
    return pieces.index(after: i)
  }

  public subscript(_ index: Int) -> TriviaPiece {
    return pieces[index]
  }
}


extension Trivia: ExpressibleByArrayLiteral {
  /// Creates Trivia from the provided pieces.
  public init(arrayLiteral elements: TriviaPiece...) {
    self.pieces = elements
  }
}

/// Concatenates two collections of `Trivia` into one collection.
public func +(lhs: Trivia, rhs: Trivia) -> Trivia {
  return Trivia(pieces: lhs.pieces + rhs.pieces)
}
