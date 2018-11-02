//===--------------- Diagnostics.swift - Diagnostic Emitter ---------------===//
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
// This file provides the Diagnostic, Note, and FixIt types.
//===----------------------------------------------------------------------===//

import Foundation

/// Represents a source location in a Swift file.
public struct SourceLocation: Codable {
  /// The line in the file where this location resides.
  public let line: Int

  /// The UTF-8 byte offset from the beginning of the line where this location
  /// resides.
  public let column: Int

  /// The UTF-8 byte offset into the file where this location resides.
  public let offset: Int

  /// The file in which this location resides.
  public let file: String

  public init(file: String, position: AbsolutePosition) {
    self.init(line: position.line, column: position.column,
              offset: position.utf8Offset, file: file)
  }

  public init(line: Int, column: Int, offset: Int, file: String) {
    self.line = line
    self.column = column
    self.offset = offset
    self.file = file
  }
}

/// Represents a start and end location in a Swift file.
public struct SourceRange: Codable {
  /// The beginning location in the source range.
  public let start: SourceLocation

  /// The beginning location in the source range.
  public let end: SourceLocation

  public init(start: SourceLocation, end: SourceLocation) {
    self.start = start
    self.end = end
  }
}

/// A FixIt represents a change to source code in order to "correct" a
/// diagnostic.
public enum FixIt: Codable {
  /// Remove the characters from the source file over the provided source range.
  case remove(SourceRange)

  /// Insert, at the provided source location, the provided string.
  case insert(SourceLocation, String)

  /// Replace the characters at the provided source range with the provided
  /// string.
  case replace(SourceRange, String)

  enum CodingKeys: String, CodingKey {
    case type
    case range
    case location
    case string
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let type = try container.decode(String.self, forKey: .type)
    switch type {
    case "remove":
      let range = try container.decode(SourceRange.self, forKey: .range)
      self = .remove(range)
    case "insert":
      let string = try container.decode(String.self, forKey: .string)
      let loc = try container.decode(SourceLocation.self, forKey: .location)
      self = .insert(loc, string)
    case "replace":
      let string = try container.decode(String.self, forKey: .string)
      let range = try container.decode(SourceRange.self, forKey: .range)
      self = .replace(range, string)
    default:
      fatalError("unknown FixIt type \(type)")
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    switch self {
    case let .remove(range):
      try container.encode(range, forKey: .range)
    case let .insert(location, string):
      try container.encode(location, forKey: .location)
      try container.encode(string, forKey: .string)
    case let .replace(range, string):
      try container.encode(range, forKey: .range)
      try container.encode(string, forKey: .string)
    }
  }

  /// The source range associated with a FixIt. If this is an insertion,
  /// it is a range with the same start and end location.
  public var range: SourceRange {
    switch self {
    case .remove(let range), .replace(let range, _): return range
    case .insert(let loc, _): return SourceRange(start: loc, end: loc)
    }
  }

  /// The text associated with this FixIt. If this is a removal, the text is
  /// the empty string.
  public var text: String {
    switch self {
    case .remove(_): return ""
    case .insert(_, let text), .replace(_, let text): return text
    }
  }
}

/// A Note attached to a Diagnostic. This provides more context for a specific
/// error, and optionally allows for FixIts.
public struct Note: Codable {
  /// The note's message.
  public let message: Diagnostic.Message

  /// The source location where the note should point.
  public let location: SourceLocation?

  /// An array of source ranges that should be highlighted.
  public let highlights: [SourceRange]

  /// An array of FixIts that apply to this note.
  public let fixIts: [FixIt]

  /// Constructs a new Note from the constituent parts.
  internal init(message: Diagnostic.Message, location: SourceLocation?,
                highlights: [SourceRange], fixIts: [FixIt]) {
    precondition(message.severity == .note,
                 "notes can only have the `note` severity")
    self.message = message
    self.location = location
    self.highlights = highlights
    self.fixIts = fixIts
  }

  /// Converts this Note to a Diagnostic for serialization.
  func asDiagnostic() -> Diagnostic {
    return Diagnostic(message: message, location: location, notes: [],
                      highlights: highlights, fixIts: fixIts)
  }
}

/// A Diagnostic message that can be emitted regarding some piece of code.
public struct Diagnostic: Codable {
  public struct Message: Codable {
    /// The severity of diagnostic. This can be note, error, or warning.
    public let severity: Severity

    /// A string containing the contents of the diagnostic.
    public let text: String

    /// Creates a diagnostic message with the provided severity and text.
    public init(_ severity: Severity, _ text: String) {
      self.severity = severity
      self.text = text
    }
  }

  // These values must match clang/Frontend/SerializedDiagnostics.h
  /// The severity of the diagnostic.
  public enum Severity: UInt8, Codable {
    case note = 1
    case warning = 2
    case error = 3
  }

  /// The diagnostic's message.
  public let message: Message

  /// The location the diagnostic should point.
  public let location: SourceLocation?

  /// An array of notes providing more context for this diagnostic.
  public let notes: [Note]

  /// An array of source ranges to highlight.
  public let highlights: [SourceRange]

  /// An array of possible FixIts to apply to this diagnostic.
  public let fixIts: [FixIt]

  /// A diagnostic builder that exposes mutating operations for notes,
  /// highlights, and FixIts. When a Diagnostic is created, a builder
  /// will be provided in a closure where the user can conditionally
  /// add notes, highlights, and FixIts, that will then be wrapped
  /// into the immutable Diagnostic object.
  public struct Builder {
    /// An in-flight array of notes.
    internal var notes = [Note]()

    /// An in-flight array of highlighted source ranges.
    internal var highlights = [SourceRange]()

    /// An in-flight array of FixIts.
    internal var fixIts = [FixIt]()

    internal init() {}

    /// Adds a Note to the diagnostic builder.
    /// - parameters:
    ///   - message: The message associated with the note. This must have the
    ///              `.note` severity.
    ///   - location: The source location to which this note is attached.
    ///   - highlights: Any source ranges that should be highlighted by this
    ///                 note.
    ///   - fixIts: Any FixIts that should be attached to this note.
    public mutating func note(_ message: Message,
                              location: SourceLocation? = nil,
                              highlights: [SourceRange] = [],
                              fixIts: [FixIt] = []) {
      self.notes.append(Note(message: message, location: location,
                             highlights: highlights, fixIts: fixIts))
    }

    /// Adds the provided source ranges as highlights of this diagnostic.
    public mutating func highlight(_ ranges: SourceRange...) {
      self.highlights += ranges
    }

    /// Adds a FixIt to remove the contents of the provided SourceRange.
    /// When applied, this FixIt will delete the characters corresponding to
    /// this range in the original source file.
    public mutating func fixItRemove(_ sourceRange: SourceRange) {
      fixIts.append(.remove(sourceRange))
    }

    /// Adds a FixIt to insert the provided text at the provided SourceLocation
    /// in the file where the location resides.
    public mutating
    func fixItInsert(_ text: String, at sourceLocation: SourceLocation) {
      fixIts.append(.insert(sourceLocation, text))
    }

    /// Adds a FixIt to replace the contents of the source file corresponding
    /// to the provided SourceRange with the provided text.
    public mutating
    func fixItReplace(_ sourceRange: SourceRange, with text: String) {
      fixIts.append(.replace(sourceRange, text))
    }
  }

  /// Creates a new Diagnostic with the provided message, pointing to the
  /// provided location (if any).
  /// This initializer also takes a closure that will be passed a Diagnostic
  /// Builder as an inout parameter. Use this closure to add notes, highlights,
  /// and FixIts to the diagnostic through the Builder's API.
  /// - parameters:
  ///   - message: The diagnostic's message.
  ///   - location: The location the diagnostic is attached to.
  ///   - actions: A closure that's used to attach notes and highlights to
  ///              diagnostics.
  init(message: Message, location: SourceLocation?,
       actions: ((inout Builder) -> Void)?) {
    var builder = Builder()
    actions?(&builder)
    self.init(message: message, location: location, notes: builder.notes,
              highlights: builder.highlights, fixIts: builder.fixIts)
  }

  /// Creates a new Diagnostic with the provided message, pointing to the
  /// provided location (if any).
  /// - parameters:
  ///   - message: The diagnostic's message.
  ///   - location: The location the diagnostic is attached to.
  ///   - highlights: An array of SourceRanges which will be highlighted when
  ///                 the diagnostic is presented.
  init(message: Message, location: SourceLocation?, notes: [Note],
       highlights: [SourceRange], fixIts: [FixIt]) {
    self.message = message
    self.location = location
    self.notes = notes
    self.highlights = highlights
    self.fixIts = fixIts
  }
}
